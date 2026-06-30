{- |
Module      : Actor.Field
Description : Flat per-output byte-array fields shared by the preparation pass
              and the continuation pass.

Both the @preparation:@ pre-pass (LLVM backend) and the @continuation:@ tile
pass store their published outputs in flat @Ptr Word8@ arrays, one per output
variable, indexed by a (global) point index. This module owns that byte-array
representation (stride, allocation, typed read/write) plus the field geometry and
serpentine traversal the continuation driver walks.

The helpers here are backend-agnostic (plain 'Foreign' code), so the pure
backend, the continuation driver, and the LLVM backend all share them.
-}
module Actor.Field
  ( -- * Byte-array storage
    prepArrayStride
  , withPrepArrays
  , writeToPrepArray
  , readFromPrepArray
  , writePrepOutputsFromMap
  , noPrepDraw
    -- * Field geometry and traversal
  , FieldGeometry(..)
  , fieldIndex
  , pointCoord
  , serpentineOrder
  , reprojectIndex
  , coarsenGeometry
    -- * Continuation driver
  , runContinuationField
  , runAutoContinuationField
  , coarsenGeometryCentered
  , overrideBoolByName
    -- * Reading a computed field at render time
  , ContinuationField(..)
  , overrideFromField
  , markHasSeed
    -- * Persistent field allocation + context helpers
  , mallocFieldArrays
  , freeContinuationField
  , overrideComplexByName
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Code (Code)
import Language.Code.InterpretIO (interpretToIOWithLastValues, ScalarIORefM, IORefTypeOfBinding)
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Typecheck (InternalStuck, InternalContSeed, InternalHasSeed)
import Language.Draw (DrawHandler(..))
import Data.Color (colorToRGB)

import Data.IORef (newIORef, writeIORef)
import Foreign hiding (void)
import qualified Data.Map.Strict as Map

-- | Stride in bytes per point for each FSType in a field array.
prepArrayStride :: TypeProxy t -> Int
prepArrayStride = \case
  BooleanType -> 1
  IntegerType -> 4
  RealType    -> 8
  ComplexType -> 16
  ColorType   -> 3
  _           -> 4  -- stub for List/Text

-- | Run @action@ with one zeroed @Ptr Word8@ array per variable in @env@.
-- The arrays are stack-allocated and zero-initialised, sized for @nPoints@.
withPrepArrays :: EnvironmentProxy env -> Int -> ([Ptr Word8] -> IO r) -> IO r
withPrepArrays EmptyEnvProxy _ action = action []
withPrepArrays (BindingProxy _name ty env') nPoints action =
  let sz = nPoints * prepArrayStride ty
  in allocaBytes sz $ \ptr -> do
    fillBytes ptr 0 sz
    withPrepArrays env' nPoints $ \restPtrs ->
      action (ptr : restPtrs)

-- | Write a single Haskell value into a field array at the given byte offset.
writeToPrepArray :: TypeProxy t -> Ptr Word8 -> Int -> HaskellType t -> IO ()
writeToPrepArray ty ptr offset val = case ty of
  BooleanType -> pokeByteOff ptr offset (if val then (1 :: Word8) else 0)
  IntegerType -> pokeByteOff ptr offset (fromIntegral val :: Int32)
  RealType    -> pokeByteOff ptr offset (val :: Double)
  ComplexType -> let re :+ im = val
                 in pokeByteOff ptr offset re >> pokeByteOff ptr (offset + 8) im
  ColorType   -> let (r, g, b) = colorToRGB val
                 in pokeByteOff ptr offset r
                 >> pokeByteOff ptr (offset + 1) g
                 >> pokeByteOff ptr (offset + 2) b
  _ -> pure ()

-- | Read a single Haskell value back from a field array at the given byte
-- offset. Inverse of 'writeToPrepArray' for the scalar types (Color and the
-- List/Text stubs are not supported and will error).
readFromPrepArray :: TypeProxy t -> Ptr Word8 -> Int -> IO (HaskellType t)
readFromPrepArray ty ptr offset = case ty of
  BooleanType -> (/= (0 :: Word8)) <$> peekByteOff ptr offset
  IntegerType -> (fromIntegral :: Int32 -> Int64) <$> peekByteOff ptr offset
  RealType    -> peekByteOff ptr offset :: IO Double
  ComplexType -> (:+) <$> (peekByteOff ptr offset :: IO Double)
                      <*> (peekByteOff ptr (offset + 8) :: IO Double)
  _ -> error "readFromPrepArray: unsupported field type"

-- | For each variable in @env@, look up its last-assigned value from the
-- interpreter's tracking map and write it to the corresponding array at
-- point index @pointIdx@.
writePrepOutputsFromMap
  :: EnvironmentProxy env
  -> [Ptr Word8]
  -> Map.Map String SomeHaskellType
  -> Int
  -> IO ()
writePrepOutputsFromMap EmptyEnvProxy [] _ _ = pure ()
writePrepOutputsFromMap (BindingProxy name ty env') (ptr:ptrs) vals pointIdx = do
  let n = symbolVal name
      byteOffset = pointIdx * prepArrayStride ty
  case Map.lookup n vals of
    Just (SomeHaskellType ty' val) -> writeToPrepArray ty' ptr byteOffset val
    Nothing                        -> pure ()
  writePrepOutputsFromMap env' ptrs vals pointIdx
writePrepOutputsFromMap _ _ _ _ = pure ()

-- | No-op draw handler for use in the Haskell prep / continuation passes.
noPrepDraw :: DrawHandler ScalarIORefM
noPrepDraw = DrawHandler (\_ -> pure ())

-- | The model-plane geometry of a field: the model coordinate of the
-- top-left point (0,0), the per-point steps, and the dimensions. Carried with
-- the field from the start so a later cross-render layer can reproject one
-- field's points onto another's grid.
data FieldGeometry = FieldGeometry
  { fgOriginX :: Double   -- ^ model x of point (col=0, row=0)
  , fgOriginY :: Double   -- ^ model y of point (col=0, row=0)
  , fgDX      :: Double   -- ^ model x-step per column (x increases L→R)
  , fgDY      :: Double   -- ^ model y-step per row (typically negative: y decreases top→bottom)
  , fgWidth   :: Int
  , fgHeight  :: Int
  } deriving (Eq, Show)

-- | Flat index of point (col, row) into a width*height field array.
fieldIndex :: FieldGeometry -> Int -> Int -> Int
fieldIndex fg col row = row * fgWidth fg + col

-- | Model coordinate of point (col, row).
pointCoord :: FieldGeometry -> Int -> Int -> Complex Double
pointCoord fg col row =
  (fgOriginX fg + fromIntegral col * fgDX fg)
  :+ (fgOriginY fg + fromIntegral row * fgDY fg)

-- | Coarsen a (full-resolution, one-point-per-pixel) field grid by a per-axis
-- factor @d@: the field then holds one point per @d x d@ block (steps @d@x larger,
-- dimensions @d@x smaller). Point 0 still sits on pixel 0, so the per-pixel read's
-- nearest-grid reprojection maps each pixel to its nearest coarse point. @d <= 1@
-- leaves the grid at full resolution.
coarsenGeometry :: Int -> FieldGeometry -> FieldGeometry
coarsenGeometry d g
  | d <= 1    = g
  | otherwise = g { fgDX = fgDX g * fromIntegral d
                  , fgDY = fgDY g * fromIntegral d
                  , fgWidth  = ceilDiv (fgWidth g)  d
                  , fgHeight = ceilDiv (fgHeight g) d }
  where ceilDiv a b = (a + b - 1) `div` b

-- | Like 'coarsenGeometry', but place each coarse point at the *centre* of its
-- @d x d@ cell (origin shifted by half a cell) instead of the cell's corner. The
-- nearest-grid read then maps every pixel to the seed inside its own cell — no
-- cross-block bleed — and the seed is the representative centre of the points it
-- serves.
coarsenGeometryCentered :: Int -> FieldGeometry -> FieldGeometry
coarsenGeometryCentered d g
  | d <= 1    = g
  | otherwise =
      let half = 0.5 * fromIntegral d
      in g { fgOriginX = fgOriginX g + half * fgDX g
           , fgOriginY = fgOriginY g + half * fgDY g
           , fgDX = fgDX g * fromIntegral d
           , fgDY = fgDY g * fromIntegral d
           , fgWidth  = ceilDiv (fgWidth g)  d
           , fgHeight = ceilDiv (fgHeight g) d }
  where ceilDiv a b = (a + b - 1) `div` b

-- | Map a model coordinate to the nearest field point's flat index, or
-- 'Nothing' if it falls outside the field grid. Inverse of 'pointCoord' (rounded
-- to the nearest grid point). This is how a per-pixel kernel reads a tile field:
-- reproject the pixel's coordinate onto the field grid.
reprojectIndex :: FieldGeometry -> Complex Double -> Maybe Int
reprojectIndex fg (x :+ y) =
  let col = round ((x - fgOriginX fg) / fgDX fg) :: Int
      row = round ((y - fgOriginY fg) / fgDY fg) :: Int
  in if col >= 0 && col < fgWidth fg && row >= 0 && row < fgHeight fg
       then Just (row * fgWidth fg + col)
       else Nothing

-- | Points of a width*height grid in serpentine (boustrophedon) order: even
-- rows left→right, odd rows right→left, so consecutive points are always
-- adjacent (one step apart). Returns (col, row) pairs.
serpentineOrder :: Int -> Int -> [(Int, Int)]
serpentineOrder width height =
  [ (col, row)
  | row <- [0 .. height - 1]
  , col <- if even row then [0 .. width - 1] else [width - 1, width - 2 .. 0]
  ]

-- | Run the continuation pass over a tile, single-threaded, in serpentine
-- order. At each point: build the point's interpreter context (binding the
-- coordinate and seeding the continued unknown), run the continuation @code@,
-- store its published @outputEnv@ outputs into @arrays@ at the point's global
-- index, and thread the published @solution@ forward as the next point's seed.
--
-- The very first point is seeded from @anchor@. Because serpentine order keeps
-- consecutive points adjacent, every later point is seeded from its immediately
-- previous (adjacent) neighbour's solution — one continuous path, no re-seeding
-- at row starts. If a point publishes no @solution@ (e.g. the code didn't solve),
-- the previous seed is carried forward unchanged.
--
-- @mkContext coord seed@ supplies the point's @Context HaskellValue env@ with the
-- coordinate bound to @coord@ and the continued unknown bound to @seed@; all
-- other bindings (config, outputs' defaults, solve bookkeeping) come from the
-- caller. @arrays@ must be one @Ptr Word8@ per variable in @outputEnv@, in env
-- order, each sized for @fgWidth*fgHeight@ points (see 'withPrepArrays').
--
-- @anchorAt coord@ supplies the seed for the very first point as a function of
-- its coordinate (e.g. @const k@ for a fixed anchor, or @id@ for @anchor: c@).
-- Failure handling: a point that did not converge (@stuck@) does not become the
-- seed for the next point — the last *converged* solution is carried forward
-- instead, so a pocket of non-convergence doesn't poison the continuation.
runContinuationField
  :: forall env outputEnv
   . EnvironmentProxy outputEnv
  -> Code env
  -> (Complex Double -> Complex Double)   -- ^ anchor as a function of the first coord
  -> (Complex Double -> Complex Double -> Context HaskellValue env)
  -> FieldGeometry
  -> [Ptr Word8]
  -> IO ()
runContinuationField outputEnv code anchorAt mkContext geom arrays =
    go Nothing (serpentineOrder (fgWidth geom) (fgHeight geom))
  where
    -- @mLastGood@ is the last converged solution (the seed for the next point);
    -- 'Nothing' before the first point (use the anchor) and whenever no good
    -- solution has been seen yet.
    go :: Maybe (Complex Double) -> [(Int, Int)] -> IO ()
    go _         []               = pure ()
    go mLastGood ((col, row):pts) = do
      let coord = pointCoord geom col row
          seed  = fromMaybe (anchorAt coord) mLastGood
      iorefs <- mapContextM (\_ _ -> newIORef) (mkContext coord seed)
      (lastVals, _) <- execStateT
        (interpretToIOWithLastValues noPrepDraw code)
        (Map.empty, iorefs)
      writePrepOutputsFromMap outputEnv arrays lastVals (fieldIndex geom col row)
      go (nextGood lastVals mLastGood) pts

    -- The continued state is the published `solution` (set by `solve`); advance
    -- it only when this point converged (`not stuck`), else keep the last good.
    nextGood :: Map.Map String SomeHaskellType
             -> Maybe (Complex Double) -> Maybe (Complex Double)
    nextGood vals lastGood
      | stuck     = lastGood
      | otherwise = case Map.lookup "solution" vals of
          Just (SomeHaskellType ComplexType v) -> Just v
          _                                    -> lastGood
      where stuck = case Map.lookup (symbolVal (Proxy @InternalStuck)) vals of
              Just (SomeHaskellType BooleanType b) -> b
              _                                    -> False

-- | The pre-pass for an automatic (`solve … continuing seed`) continuation: run
-- the *viewer code* itself at each (centred, coarse) cell in serpentine order,
-- threading the continued seed across cells, and store each cell's solution.
--
-- @mkContext coord mSeed@ builds the cell's context: @mSeed = Just s@ for a
-- threaded cell (set @[internal] continuation seed@ = @s@ and @… has seed@ =
-- True) or @Nothing@ for a cold-start cell (@has seed@ = False, so the solve uses
-- the unknown's current value as the anchor). The field's single output is
-- @[internal] continuation seed@, which the continuing-solve lowering overwrites
-- with this cell's solution (NaN if it didn't converge). A non-converged cell is
-- stored as NaN but does not poison the thread — the last converged solution is
-- carried forward.
runAutoContinuationField
  :: forall env outputEnv
   . EnvironmentProxy outputEnv
  -> Code env
  -> (Complex Double -> Maybe (Complex Double) -> Context HaskellValue env)
  -> FieldGeometry
  -> [Ptr Word8]
  -> IO ()
runAutoContinuationField outputEnv code mkContext geom arrays =
    go Nothing (serpentineOrder (fgWidth geom) (fgHeight geom))
  where
    go :: Maybe (Complex Double) -> [(Int, Int)] -> IO ()
    go _         []               = pure ()
    go mLastGood ((col, row):pts) = do
      iorefs <- mapContextM (\_ _ -> newIORef)
                  (mkContext (pointCoord geom col row) mLastGood)
      (lastVals, _) <- execStateT
        (interpretToIOWithLastValues noPrepDraw code)
        (Map.empty, iorefs)
      writePrepOutputsFromMap outputEnv arrays lastVals (fieldIndex geom col row)
      go (nextGood lastVals mLastGood) pts

    -- Thread the captured solution (the field output @contSeed@) forward, unless
    -- it is NaN (did not converge), in which case keep the last good one.
    nextGood :: Map.Map String SomeHaskellType
             -> Maybe (Complex Double) -> Maybe (Complex Double)
    nextGood vals lastGood = case Map.lookup (symbolVal (Proxy @InternalContSeed)) vals of
      Just (SomeHaskellType ComplexType v)
        | not (isNaN (realPart v)) -> Just v
      _                            -> lastGood

-- | A computed continuation field handed to the render pass: the published
-- output variables, one flat byte array per output (env order), and the grid
-- geometry. The per-pixel kernel reprojects its coordinate onto this grid and
-- reads the outputs.
data ContinuationField where
  ContinuationField :: EnvironmentProxy outputEnv  -- ^ published outputs
                    -> [Ptr Word8]                 -- ^ one array per output, env order
                    -> FieldGeometry
                    -> ContinuationField

-- | Override the continuation output bindings in an interpreter context from the
-- field at point index @idx@. For each output variable, read its stored value and
-- write it into the matching binding (located by name in the full render env).
-- Bindings not present in the render env are skipped.
overrideFromField
  :: forall fullEnv outputEnv
   . EnvironmentProxy fullEnv
  -> EnvironmentProxy outputEnv
  -> [Ptr Word8]
  -> Int
  -> Context IORefTypeOfBinding fullEnv
  -> IO ()
overrideFromField fullEnv outputEnv arrays idx ctx = go outputEnv arrays
  where
    go :: forall e. EnvironmentProxy e -> [Ptr Word8] -> IO ()
    go (BindingProxy name ty env') (ptr : ptrs) = do
      val <- readFromPrepArray ty ptr (idx * prepArrayStride ty)
      case lookupEnv name ty fullEnv of
        Found pf -> withKnownType ty $ writeIORef (getBinding ctx pf) val
        _        -> pure ()
      go env' ptrs
    go _ _ = pure ()

-- | Set the continuation @hasSeed@ flag to True in a render context (called per
-- pixel when a field is present, so the continuing-solve seeds from the field's
-- value rather than falling back to the cold-start anchor). No-op if the viewer
-- has no continuing solve (the variable is still in the env, just unused).
markHasSeed :: EnvironmentProxy fullEnv -> Context IORefTypeOfBinding fullEnv -> IO ()
markHasSeed fullEnv ctx = case lookupEnv (Proxy @InternalHasSeed) BooleanType fullEnv of
  Found pf -> writeIORef (getBinding ctx pf) True
  _        -> pure ()

-- | Allocate one zeroed @Ptr Word8@ array per variable in @env@, sized for
-- @nPoints@. Unlike 'withPrepArrays' (stack-scoped), these are heap-allocated
-- and persist until 'freeContinuationField' is called — needed because the field
-- is read during the (asynchronous) block renders that run after the pass.
mallocFieldArrays :: EnvironmentProxy env -> Int -> IO [Ptr Word8]
mallocFieldArrays EmptyEnvProxy _ = pure []
mallocFieldArrays (BindingProxy _name ty env') nPoints = do
  let sz = nPoints * prepArrayStride ty
  ptr <- mallocBytes sz
  fillBytes ptr 0 sz
  (ptr :) <$> mallocFieldArrays env' nPoints

-- | Free the heap arrays backing a continuation field. The caller must ensure
-- no render worker is still reading the field (free only after the owning tile's
-- worker has terminated).
freeContinuationField :: ContinuationField -> IO ()
freeContinuationField (ContinuationField _ arrays _) = mapM_ free arrays

-- | Replace the value of the complex-typed binding named @nm@ in a runtime
-- context (used to seed the continued unknown). Non-matching and non-complex
-- bindings are left unchanged.
overrideComplexByName
  :: String -> Complex Double -> Context HaskellValue e -> Context HaskellValue e
overrideComplexByName nm v = \case
  EmptyContext -> EmptyContext
  Bind name ty val rest -> case ty of
    ComplexType | symbolVal name == nm -> Bind name ty v    (overrideComplexByName nm v rest)
    _                                  -> Bind name ty val  (overrideComplexByName nm v rest)

-- | Replace the value of the boolean-typed binding named @nm@ in a runtime
-- context (used to set the continuation @hasSeed@ flag per cell).
overrideBoolByName
  :: String -> Bool -> Context HaskellValue e -> Context HaskellValue e
overrideBoolByName nm v = \case
  EmptyContext -> EmptyContext
  Bind name ty val rest -> case ty of
    BooleanType | symbolVal name == nm -> Bind name ty v   (overrideBoolByName nm v rest)
    _                                  -> Bind name ty val (overrideBoolByName nm v rest)
