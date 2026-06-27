{-# language OverloadedStrings, ForeignFunctionInterface, AllowAmbiguousTypes, UndecidableInstances, RankNTypes #-}
module Backend.LLVM
  ( JITFun(..)
  , ToForeignFun(..)
  , type LLVMJit
  , invoke
  , invoke'
  -- , withCompiledCode
  , withJIT
  , withJittedViewer
  , runJX
  , type JX
  , mkKernelFun
  , type KernelFun
  ) where

import qualified Data.ByteString.Char8 as BS

import FractalStream.Prelude

import LLVM.Module
import LLVM.Context hiding (Context)
import LLVM.PassManager
import LLVM.OrcJIT
import LLVM.Target
import LLVM.Linking
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Relocation as Reloc
import Control.Concurrent.MVar

import Foreign.LibFFI
import Foreign.C.Types

import Backend.LLVM.Code

import Language.Value
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform
import Language.Code
import Language.Code.InterpretIO (interpretToIOWithLastValues)
import Actor.Viewer
import Actor.Field
  (withPrepArrays, writePrepOutputsFromMap, noPrepDraw,
   ContinuationField(..), FieldGeometry(..))
import Data.Color

import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map

import Foreign hiding (void)

import Text.Disassembler.X86Disassembler

data JITFun (env :: Environment) (ret :: FSType) where
  JITFun :: EnvironmentProxy env -> TypeProxy ret -> FunPtr () -> JITFun env ret

type JX = FunPtr (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())

runJX :: (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())
      -> Ptr Word8 -> Int32 -> Int32 -> Complex Double -> Int32 -> Double -> Complex Double -> IO ()
runJX go outPtr blockSize subsamples (dx :+ dy) maxIters maxRadius (x :+ y) = do
  allocaArray @Double 2 $ \dz -> do
      pokeArray dz [dx,dy]
      go outPtr blockSize subsamples dz maxIters maxRadius x y

{-
foreign import ccall "dynamic"
  mkJX :: JX -> Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()
-}

type KernelFun =  Ptr Word8 -- output buffer of 8-bit / channel rgb triples
               -> Int32 -- width and height of block to generate
               -> Int32 -- number of subsamples per output pixel
               -> Double -- step size dx
               -> Double -- step size dy
               -> Double -- initial x value
               -> Double -- initial y value
               -> Ptr () -- opaque context
               -> IO ()

foreign import ccall "dynamic"
  mkKernelFun :: FunPtr KernelFun -> KernelFun

class ToForeignFun (env :: Environment) (ret :: FSType) where
  type AsForeignFun env ret :: *
  toForeignFun :: (Context HaskellValue env -> IO (HaskellType ret))
               -> AsForeignFun env ret

instance ToForeignFun '[] ret where
  type AsForeignFun '[] ret = IO (HaskellType ret)
  toForeignFun f = f EmptyContext

instance (KnownSymbol name, KnownType t, ToForeignFun env ret, NotPresent name env)
    => ToForeignFun ( '(name,t) ': env) ret where
  type AsForeignFun ( '(name,t) ': env) ret = HaskellType t -> AsForeignFun env ret
  toForeignFun f x = toForeignFun @env @ret (f . Bind (Proxy @name) (typeProxy @t) x)

invoke :: JITFun env ret -> Context HaskellValue env -> IO (HaskellType ret)
invoke (JITFun _ rt f) ctx = do
  (args, frees) <- unzip <$> fromContextM toFFIArg ctx
  allocaArray @Double 2 $ \ret -> do   -- FIXME: allocate the correct type!
    callFFI f retVoid (argPtr ret : args)
    sequence_ frees
    fromFFIRetArg rt ret

invoke' :: forall env ret
         . ToForeignFun env ret
        => EnvironmentProxy env
        -> TypeProxy ret
        -> JITFun env ret
        -> AsForeignFun env ret
invoke' _ _ f = toForeignFun @env @ret (invoke f)

toFFIArg :: Proxy (name :: Symbol)
         -> TypeProxy ty
         -> HaskellType ty
         -> IO (Arg, IO ())
toFFIArg _ t v = case t of
  IntegerType -> pure (argInt32 (fromIntegral v), pure ())
  RealType    -> pure (argCDouble (CDouble v), pure ())
  ComplexType -> do
    let x :+ y = v
    z <- mallocArray 2
    pokeArray z [x,y]
    pure (argPtr z, free z)
  ColorType -> do
    let (r, g, b) = colorToRGB v
    c <- mallocArray 3
    pokeArray c [r,g,b]
    pure (argPtr c, free c)
  ListType _ -> pure (argInt32 0, pure ())
  TextType -> pure (argInt32 0, pure ())
  BooleanType -> pure (argInt8 (if v then 1 else 0), pure ())
  _ -> error ("todo: toFFIArg " ++ showType t)

fromFFIRetArg :: TypeProxy ty
              -> Ptr Double
              -> IO (HaskellType ty)
fromFFIRetArg t ptr = case t of
  IntegerType -> fromIntegral <$> peek (castPtr @_ @Int32 ptr)
  RealType    -> peek (castPtr ptr)
  ComplexType -> do
    [x,y] <- peekArray 2 (castPtr ptr)
    pure (x :+ y)
  ColorType -> do
    [cr,cg,cb] <- peekArray 3 (castPtr ptr)
    pure (rgbToColor (cr, cg, cb))
  BooleanType -> do
    v <- peek (castPtr @_ @Int8 ptr)
    pure (v /= 0)
  _ -> error ("todo: fromFFIRetArg " ++ showType t)

-- | CPS wrapper that exposes the prep output environment proxy from a 'PrepScript'.
-- Avoids existential escape by keeping the proxy in the continuation's scope.
withPrepEnvProxy :: Maybe (PrepScript env)
                 -> (forall prepOutputEnv. EnvironmentProxy prepOutputEnv -> IO r)
                 -> IO r
withPrepEnvProxy Nothing                       k = k EmptyEnvProxy
withPrepEnvProxy (Just (PrepScript proxy _))   k = k proxy

-- | Like 'withPrepEnvProxy' but for the continuation output environment.
withContEnvProxy :: Maybe (ContinuationScript env)
                 -> (forall contOutputEnv. EnvironmentProxy contOutputEnv -> IO r)
                 -> IO r
withContEnvProxy Nothing                              k = k EmptyEnvProxy
withContEnvProxy (Just (ContinuationScript proxy _ _ _ _)) k = k proxy

withJittedViewer :: forall env t. (MissingViewerArgs env, KnownEnvironment env)
                 => LLVMJit
                 -> Maybe (PrepScript env)
                 -> Maybe (ContinuationScript env)
                 -> Code (ViewerEnv env)
                 -> (ViewerFunction env -> IO t) -> IO t
withJittedViewer (dylib, session, compileLayer, nextId) mPrepScript mContScript code0 action = do
  -- Do some basic AST-level optimizations first
  let code = transformValues (integerPowers . avoidSqrt) code0
  name <- modifyMVar nextId (\n -> pure (n + 1, "kernel_" ++ show n))
  withPrepEnvProxy mPrepScript $ \prepEnvProxy ->
   withContEnvProxy mContScript $ \contEnvProxy -> do
    m <- either error pure (compileRenderer' prepEnvProxy contEnvProxy (fromString name) code)
    withContext $ \ctx ->
      withModuleFromAST ctx m $ \md -> do
      let pm = CuratedPassSetSpec
               { optLevel = Just 2 -- "-O2"?
               , sizeLevel = Nothing
               , unitAtATime = Nothing
               , simplifyLibCalls = Just True
               , loopVectorize = Just True
               , superwordLevelParallelismVectorize = Nothing
               , useInlinerWithThreshold = Nothing
               , dataLayout = Nothing
               , targetLibraryInfo = Nothing
               , targetMachine = Nothing
               }
      withPassManager pm (`runPassManager` md)

      let dumpLLVM = False
          dumpAsm  = False
      when dumpLLVM $ do
        putStrLn "------------------------------------------------------------"
        asm' <- BS.unpack <$> moduleLLVMAssembly md
        putStrLn asm'

      withClonedThreadSafeModule md $ \tsm -> do
        addModule tsm dylib compileLayer
        lookupSymbol session compileLayer dylib (fromString name) >>= \case
          Left err -> error ("error JITing kernel: " ++ show err)
          Right (JITSymbol kernelFn _) -> do
            when dumpLLVM $
              putStrLn "------------------------------------------------------------"

            when dumpAsm $ do
              let dcfg = defaultConfig { confIn64BitMode = True }
              instrs <- disassembleBlockWithConfig dcfg (wordPtrToPtr kernelFn) 1024
              case instrs of
                Left err -> putStrLn ("disassembly error: " ++ show err)
                Right is -> forM_ is (\i -> putStrLn ("  " ++ showIntel i))

            let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
            action $ ViewerFunction $ \ViewerArgs{..} -> do
              (colorArg, colorFree) <- toFFIArg (Proxy @"color") ColorType grey
              (args, frees) <- unzip <$> fromContextM toFFIArg vaArgs
              let nPixels = fromIntegral vaWidth * fromIntegral vaHeight :: Int
                  w = fromIntegral vaWidth  :: Int
                  h = fromIntegral vaHeight :: Int
                  (x0, y0) = vaPoint
                  (dx, dy) = vaStep
              withPrepArrays prepEnvProxy nPixels $ \prepPtrs -> do
                -- Haskell prep pass: populate prep arrays before LLVM kernel
                case mPrepScript of
                  Nothing -> pure ()
                  Just (PrepScript _ prepCode) -> do
                    forM_ (zip [0 .. h - 1] [y0, y0 - dy ..]) $ \(row, y) ->
                      forM_ (zip [0 .. w - 1] [x0, x0 + dx ..]) $ \(col, x) -> do
                        let pixelCtx :: Context HaskellValue (ViewerEnv env)
                            pixelCtx = Bind (Proxy @InternalX)  RealType  x
                                     $ Bind (Proxy @InternalY)  RealType  y
                                     $ Bind (Proxy @InternalDX) RealType  dx
                                     $ Bind (Proxy @InternalDY) RealType  dy
                                     $ Bind (Proxy @"color")    ColorType grey
                                     $ vaArgs
                        iorefs <- mapContextM (\_ _ -> newIORef) pixelCtx
                        (lastVals, _) <- execStateT
                          (interpretToIOWithLastValues noPrepDraw prepCode)
                          (Map.empty, iorefs)
                        writePrepOutputsFromMap prepEnvProxy prepPtrs lastVals
                          (row * w + col)
                -- Call the LLVM kernel with prep + continuation arrays passed as
                -- raw pointers, followed by the field grid geometry. @contPtrs@
                -- and the geometry come from the tile's continuation field (or, if
                -- none, safe zeroed dummy arrays + a 1x1 grid so every pixel clamps
                -- to index 0 and reads the output defaults).
                let runWithCont contPtrs (gx, gy, gdx, gdy, gw, gh) =
                      callFFI fn retVoid $
                        argPtr   vaBuffer
                        : argInt32 vaWidth
                        : argInt32 vaHeight
                        : argInt32 vaSubsamples
                        : argCDouble (CDouble $ fst vaPoint)
                        : argCDouble (CDouble $ snd vaPoint)
                        : argCDouble (CDouble $ fst vaStep)
                        : argCDouble (CDouble $ snd vaStep)
                        : colorArg
                        : args ++ map argPtr prepPtrs ++ map argPtr contPtrs
                       ++ [ argCDouble (CDouble gx), argCDouble (CDouble gy)
                          , argCDouble (CDouble gdx), argCDouble (CDouble gdy)
                          , argInt32 gw, argInt32 gh ]
                case vaContinuationField of
                  Just (ContinuationField _ arrays geom) ->
                    runWithCont arrays
                      ( fgOriginX geom, fgOriginY geom, fgDX geom, fgDY geom
                      , fromIntegral (fgWidth geom), fromIntegral (fgHeight geom) )
                  Nothing ->
                    withPrepArrays contEnvProxy 1 $ \dummy ->
                      runWithCont dummy (0, 0, 1, 1, 1, 1)
              sequence_ (colorFree : frees)

{-
-- This is only used in tests
withCompiledCode :: forall env
                  . ( KnownEnvironment env
                    , Required "x" env ~ 'RealT
                    , NotPresent "x" (env `Without` "x")
                    , Required "y" env ~ 'RealT
                    , NotPresent "y" (env `Without` "y")
                    , Required "color" env ~ 'ColorT
                    , NotPresent "color" (env `Without` "color")
                    , NotPresent "#blockSize" env
                    , NotPresent "#subsamples" env
                    , NotPresent "#dz" env
                    )
                 => EnvironmentProxy env
                 -> String
                 -> ((Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()) -> IO ())
                 -> IO ()
withCompiledCode env code run = do
  c <- case parseCode env Map.empty code of
         Left e  -> error (ppFullError e code)
         Right c -> pure c
  m <- either error pure (compileRenderer c)
  loadLibraryPermanently Nothing
  withContext $ \ctx ->
    withModuleFromAST ctx m $ \md -> do
      let pm = defaultCuratedPassSetSpec
      withPassManager pm (`runPassManager` md)
      asm' <- BS.unpack <$> moduleLLVMAssembly md
      putStrLn asm'

      withHostTargetMachine' $ \tm -> do
        withExecutionSession $ \session -> do
          withClonedThreadSafeModule md $ \tsm -> do
            let dylibName = "kernel_dylib"
            dylib <- createJITDylib session dylibName
            linker <- createRTDyldObjectLinkingLayer session --resolve
            compileLayer <- createIRCompileLayer session linker tm
            addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            addModule tsm dylib compileLayer
            lookupSymbol session compileLayer dylib "kernel" >>= \case
              Left err -> error ("error JITing kernel: " ++ show err)
              Right (JITSymbol kernelFn _) -> do
                let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
                run (mkJX fn)
-}

type LLVMJit = (JITDylib, ExecutionSession, IRCompileLayer, MVar Int)

withJIT :: (LLVMJit -> IO t) -> IO t
withJIT action = do
  _ <- loadLibraryPermanently Nothing
  withHostTargetMachine' $ \tm -> do
    withExecutionSession $ \session -> do
      let dylibName = "kernel_dylib"
      dylib <- createJITDylib session dylibName
      linker <- createRTDyldObjectLinkingLayer session --resolve
      compileLayer <- createIRCompileLayer session linker tm
      addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
      nextId <- newMVar 0
      action (dylib, session, compileLayer, nextId)

withHostTargetMachine' :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine' f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.JITDefault CodeGenOpt.Default f
