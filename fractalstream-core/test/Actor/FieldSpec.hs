{-# language QuasiQuotes #-}
module Actor.FieldSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Typecheck
  (InternalSolution, InternalIterations, InternalStuck, InternalIterationLimit)
import Language.Code.Parser (parseCode, noSplices)
import Language.Parser (ppFullError)
import Actor.Field

import Data.IORef (newIORef, readIORef)

-- | Env for the overrideFromField test: the two outputs plus an extra variable
-- (`other`) that is NOT an output and must be left untouched.
type OverrideEnv =
  '[ '("root",      'ComplexT)
   , '("converged", 'BooleanT)
   , '("other",     'ComplexT)
   ]

-- | Continuation env: coordinate @c@, continued unknown @w@, the two published
-- outputs, and the solve bookkeeping the desugaring needs.
type ContEnv =
  '[ '("c",         'ComplexT)
   , '("w",         'ComplexT)
   , '("root",      'ComplexT)
   , '("converged", 'BooleanT)
   , '(InternalSolution,       'ComplexT)
   , '(InternalIterations,     'IntegerT)
   , '(InternalStuck,          'BooleanT)
   , '(InternalIterationLimit, 'IntegerT)
   ]

-- | The published outputs, in array order.
type OutputEnv =
  '[ '("root",      'ComplexT)
   , '("converged", 'BooleanT)
   ]

-- | Continue a root of @w^2 - w + c = 0@ (the α-fixed point of @z^2 + c@).
contCode :: String
contCode = [r|solve w -> w^2 - w + c
root <- solution
converged <- not stuck
|]

-- | The point's interpreter context: coordinate bound to @cc@, unknown seeded
-- with @ww@, everything else default-initialised.
mkContext :: Complex Double -> Complex Double -> Context HaskellValue ContEnv
mkContext cc ww =
    Bind (Proxy @"c")         ComplexType cc
  $ Bind (Proxy @"w")         ComplexType ww
  $ Bind (Proxy @"root")      ComplexType 0
  $ Bind (Proxy @"converged") BooleanType False
  $ Bind (Proxy @InternalSolution)       ComplexType 0
  $ Bind (Proxy @InternalIterations)     IntegerType 0
  $ Bind (Proxy @InternalStuck)          BooleanType False
  $ Bind (Proxy @InternalIterationLimit) IntegerType 100
  $ EmptyContext

-- A small tile that straddles a row boundary. The near-0 root of w^2-w+c stays
-- well away from the other root (≈1) across this region, so a branch flip would
-- be glaring.
geom :: FieldGeometry
geom = FieldGeometry
  { fgOriginX = -0.05, fgOriginY = 0.05
  , fgDX = 0.02, fgDY = -0.02
  , fgWidth = 4, fgHeight = 3 }

-- A nonzero anchor on the near-0 branch. (Seeding exactly at 0 is singular here:
-- the language's `^` is complex power, so `0^2` evaluates via `exp(2·log 0)` to
-- NaN. Avoiding singular anchors is exactly the concern M4 will handle.)
anchor :: Complex Double
anchor = 0.1 :+ 0.1

spec :: Spec
spec = do
 describe "continuation field pass (serpentine)" $
  it "produces correct roots that stay continuous across the row seam" $
    case parseCode (envProxy (Proxy @ContEnv)) noSplices contCode of
      Left e     -> expectationFailure (ppFullError e contCode)
      Right code ->
        withPrepArrays (envProxy (Proxy @OutputEnv)) nPoints $ \arrays -> do
          runContinuationField (envProxy (Proxy @OutputEnv))
                               code (const anchor) mkContext geom arrays
          rootPtr <- case arrays of
            (p : _) -> pure p
            []      -> error "no field arrays allocated"
          roots <- forM [0 .. nPoints - 1] $ \idx ->
            readFromPrepArray ComplexType rootPtr (idx * prepArrayStride ComplexType)

          -- (1) Correctness: each stored root solves w^2 - w + c = 0 at its point.
          forM_ (zip [0 ..] roots) $ \(idx, rt) -> do
            let cc = pointCoord geom (idx `mod` w) (idx `div` w)
                residual = magnitude (rt*rt - rt + cc)
            unless (residual < 1.0e-6) $
              expectationFailure $ "point " ++ show idx
                ++ " coord=" ++ show cc ++ " root=" ++ show rt
                ++ " residual=" ++ show residual

          -- (2) Seam: (w-1,0) and (w-1,1) are the serpentine turn — vertically
          -- adjacent and consecutive in the walk. Same branch ⇒ Δ ~ O(pixel),
          -- not the ~1.0 gap to the other root.
          let at col row = roots !! (row * w + col)
          magnitude (at (w-1) 0 - at (w-1) 1) `shouldSatisfy` (< 0.05)

          -- (3) Sanity: we followed the near-0 branch from the anchor.
          magnitude (at 0 0) `shouldSatisfy` (< 0.2)

 describe "reprojectIndex" $ do
  it "maps grid coordinates to their flat field index" $ do
    reprojectIndex geom ((-0.05) :+ 0.05) `shouldBe` Just 0  -- col 0, row 0
    reprojectIndex geom (0.01    :+ 0.05) `shouldBe` Just 3  -- col 3, row 0
    reprojectIndex geom ((-0.05) :+ 0.03) `shouldBe` Just 4  -- col 0, row 1
  it "returns Nothing for coordinates outside the grid" $
    reprojectIndex geom (1.0 :+ 1.0) `shouldBe` Nothing

 describe "overrideFromField" $
  it "writes output values into matching bindings and leaves others alone" $
    withPrepArrays (envProxy (Proxy @OutputEnv)) 1 $ \arrays ->
      case arrays of
        [rootPtr, convPtr] -> do
          writeToPrepArray ComplexType rootPtr 0 (3 :+ 4)
          writeToPrepArray BooleanType convPtr 0 True
          rootRef  <- newIORef (0 :+ 0 :: Complex Double)
          convRef  <- newIORef False
          otherRef <- newIORef (9 :+ 9 :: Complex Double)
          let ctx = Bind (Proxy @"root")      ComplexType rootRef
                  $ Bind (Proxy @"converged") BooleanType convRef
                  $ Bind (Proxy @"other")     ComplexType otherRef
                  $ EmptyContext
          overrideFromField (envProxy (Proxy @OverrideEnv))
                            (envProxy (Proxy @OutputEnv)) arrays 0 ctx
          readIORef rootRef  >>= (`shouldBe` (3 :+ 4))
          readIORef convRef  >>= (`shouldBe` True)
          readIORef otherRef >>= (`shouldBe` (9 :+ 9))  -- untouched (not an output)
        _ -> expectationFailure "expected exactly two field arrays"
  where
    w       = fgWidth geom
    nPoints = fgWidth geom * fgHeight geom
