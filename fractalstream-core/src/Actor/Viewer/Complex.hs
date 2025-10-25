{-# language OverloadedStrings #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
  , ViewerUIProperties(..)
  , ComplexViewer'(..)
  , ComplexViewerCompiler(..)
  , withComplexViewer'
  , cloneComplexViewer
  , BadProject(..)
  ) where

import FractalStream.Prelude

import Actor.Layout
import Actor.Configuration
import Actor.Tool
import Actor.Event

import Language.Type
import Language.Code
import Data.DynamicValue
import Data.Color (grey)

import Language.Draw
import Language.Code.InterpretIO (ScalarIORefM, IORefTypeOfBinding, eval)

import Language.Value.Parser
import Language.Value.Typecheck (internalVanishingRadius, internalEscapeRadius)
import Language.Code.Parser

import Language.Value.Evaluator

import Foreign (Ptr)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Exception (Exception(..), throwIO)

data BadProject = BadProject String String
  deriving Show

instance Exception BadProject

data ComplexViewer = ComplexViewer
  { cvTitle :: String
  , cvSize :: (Int, Int)
  , cvCanResize :: Bool
  , cvCenter :: StringOf 'ComplexT
  , cvPixelSize :: StringOf 'RealT
  , cvCoord :: String
  , cvPixel :: Maybe String
  , cvEscapeRadius :: Maybe String
  , cvVanishRadius :: Maybe String
  , cvIterationLimit :: Maybe String
  , cvCode :: String
  , cvOverlay :: Maybe String
  , cvTools :: [ComplexTool]
  }
  deriving Show

instance FromJSON ComplexViewer where
  parseJSON = withObject "complex viewer" $ \o -> do
    cvTitle <- o .: "title"
    Dimensions cvSize <- o .: "size"
    cvCanResize <- o .:? "resizable" .!= True
    cvCoord <- o .: "z-coord"
    cvPixel <- o .:? "pixel-size"
    cvEscapeRadius <- o .:? "escape-radius"
    cvVanishRadius <- o .:? "vanishing-radius"
    cvIterationLimit <- o .:? "iteration-limit"
    StringOrNumber cvCenter <- o .: "initial-center"
    StringOrNumber cvPixelSize <- o .: "initial-pixel-size"
    cvCode <- Text.unpack <$> o .: "code"
    cvOverlay <- o .:? "overlay"
    cvTools <- (o .:? "tools" .!= []) >>= (either fail pure . sequence . map ($ cvCoord))
    pure ComplexViewer{..}


data ViewerUIProperties = ViewerUIProperties
  { vpTitle :: String
  , vpSize :: (Int, Int)
  , vpCanResize :: Bool
  }

type InternalX  = "[internal] x"
type InternalY  = "[internal] y"
type InternalPx = "[internal] px"

data ComplexViewer' where
  ComplexViewer' :: forall z px env
    . (KnownSymbol z, KnownSymbol px) =>
    { cvCenter'    :: UIValue (Complex Double)
    , cvPixelSize' :: UIValue Double
    , cvConfig' :: Context DynamicValue env
    , cvCoord' :: Proxy z
    , cvPixel' :: Proxy px
    , cvCode' :: Code
                   ( '(px, 'RealT)
                  ': '(z, 'ComplexT)
                  ': '(InternalX, 'RealT)
                  ': '(InternalY, 'RealT)
                  ': '(InternalPx, 'RealT)
                  ': '("color", 'ColorT)
                  ': env)
    , cvTools' :: [Tool]
    , cvGetDrawCommands :: IO [[DrawCommand]]
    , cvDrawCommandsChanged :: IO Bool
    , cvGetFunction :: IO (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
    } -> ComplexViewer'

cloneComplexViewer :: ComplexViewer' -> IO ComplexViewer'
cloneComplexViewer cv = do
  newCenter <- newUIValue =<< getUIValue (cvCenter' cv)
  newPixelSize <- newUIValue =<< getUIValue (cvPixelSize' cv)
  pure (cv { cvCenter' = newCenter
           , cvPixelSize' = newPixelSize
           })

newtype StringOf (t :: FSType) =
  StringOf { valueOf :: Either String (HaskellType t) }

instance KnownType t => Show (StringOf t) where
  show (StringOf v) = either id (showValue (typeProxy @t)) v

instance KnownType t => IsString (StringOf t) where
  fromString s =
    case parseValue @_ @t Map.empty s of
      Left err -> StringOf (Left $ ppFullError err s)
      Right v  -> StringOf (Right $ evaluate v EmptyContext)

instance KnownType t => FromJSON (StringOf t) where
  parseJSON = fmap unStringOrNumber . parseJSON

bindContextIO :: KnownSymbol name
              => Proxy name
              -> TypeProxy ty
              -> Eval (a name ty)
              -> Context a env
              -> IO (Context a ( '(name, ty) ': env))
bindContextIO name ty v ctx =
  case lookupEnv name ty (contextToEnv ctx) of
    Absent pf -> recallIsAbsent pf (pure (Bind name ty v ctx))
    _ -> throwIO (BadProject (symbolVal name ++ " is defined twice") [])

declareIO :: forall a name ty env
           . KnownSymbol name
          => Proxy name
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (NotPresent name env => EnvironmentProxy ( '(name,ty) ': env ) -> IO a)
          -> IO a
declareIO name ty env k =
  case lookupEnv name ty env of
    Absent pf -> recallIsAbsent pf (k $ BindingProxy name ty env)
    _ -> throwIO (BadProject (symbolVal name ++ " is defined twice") [])

withComplexViewer' :: forall env
                    . ( NotPresent "[internal argument] #blockWidth" env
                      , NotPresent "[internal argument] #blockHeight" env
                      , NotPresent "[internal argument] #subsamples" env
                      , NotPresent "color" env )
                   => ComplexViewerCompiler
                   -> Context DynamicValue env
                   -> Splices
                   -> ComplexViewer
                   -> (  ViewerUIProperties
                      -> ComplexViewer'
                      -> IO ())
                   -> IO ()
withComplexViewer' jit cvConfig' splices0 ComplexViewer{..} action = withEnvironment (contextToEnv cvConfig') $ do

  escapeRadius <- case cvEscapeRadius of
    Nothing -> pure Nothing
    Just x -> case parseParsedValue Map.empty x of
      Right v  -> pure (Just v)
      Left err -> throwIO (BadProject "I couldn't parse the viewer's escape-radius field."
                            (ppFullError err x))

  vanishRadius <- case cvVanishRadius of
    Nothing -> pure Nothing
    Just x -> case parseParsedValue Map.empty x of
      Right v  -> pure (Just v)
      Left err -> throwIO (BadProject "I couldn't parse the viewer's vanishing-radius field."
                            (ppFullError err x))

  let cvPixelName = fromMaybe "#pixel" cvPixel
      argX = Proxy @InternalX
      argY = Proxy @InternalY
      argPx = Proxy @InternalPx
      argOutput = Proxy @"color"

      splices = Map.unions $ concat
                [ [Map.singleton internalEscapeRadius x    | x <- maybeToList escapeRadius]
                , [Map.singleton internalVanishingRadius x | x <- maybeToList vanishRadius]
                , [splices0] ]

  case (someSymbolVal cvCoord, someSymbolVal cvPixelName) of
    (SomeSymbol (cvCoord' :: Proxy cvCoord'), SomeSymbol (cvPixel' :: Proxy cvPixel')) -> do
     let envOrig = contextToEnv cvConfig'

     declareIO argOutput ColorType envOrig
       $ \env0 -> declareIO argPx RealType env0
       $ \env1 -> declareIO argY RealType env1
       $ \env2 -> declareIO argX RealType env2
       $ \env3 -> declareIO cvCoord' ComplexType env3
       $ \env4 -> declareIO cvPixel' RealType env4 $ \env -> do

      cvCenter' <- case valueOf cvCenter of
        Right v  -> newUIValue v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-center field." err)
      cvPixelSize' <- case valueOf cvPixelSize of
        Right v  -> newUIValue v
        Left err -> throwIO (BadProject "I couldn't parse the viewer's initial-pixel-size field" err)

      let vpTitle = cvTitle
          vpSize  = cvSize
          vpCanResize = cvCanResize

      case parseCode env splices cvCode of
        Left err -> throwIO (BadProject "there was an error parsing the viewer's script" (ppFullError err cvCode))
        Right cvCode' -> do
          Found pfX <- pure (lookupEnv argX RealType env3)
          Found pfY <- pure (lookupEnv argY RealType env3)
          Found pfPx <- pure (lookupEnv argPx RealType env4)
          let realCode = complexToReal @_ @cvCoord' @cvPixel' pfX pfY pfPx cvCode'

          withCompiledComplexViewer jit argX argY argPx argPx argOutput realCode $ \fun -> do
            let cvGetFunction = do
                  args <- mapContextM (\_ _ -> getDynamic) cvConfig'
                  pure $ \blockWidth blockHeight subsamples (dx :+ _dy) (x :+ y) buf -> do
                    let fullArgs = Bind argX RealType x
                                 $ Bind argY RealType y
                                 $ Bind argPx RealType dx
                                 $ Bind argOutput ColorType grey
                                 $ args
                    fun (fromIntegral blockWidth) (fromIntegral blockHeight)
                        (fromIntegral subsamples) fullArgs buf

            -- For tools, bind the viewer coordinate to the
            -- view's center point. Maybe this is going to be too
            -- confusing...
            inheritedContext <- bindContextIO cvCoord' ComplexType
                                (SomeDynamic cvCenter')
                            =<< bindContextIO cvPixel' RealType
                                (SomeDynamic cvPixelSize')
                                cvConfig'
            (cvGetDrawCommands, cvDrawCommandsChanged, drawTo) <- makeDrawCommandGetter

            cvTools' <- forM cvTools $ \(ComplexTool ParsedTool{..}) -> do

              toolConfig <- case ptoolConfig of
                Nothing -> pure Nothing
                Just cfg -> fmap Just . runExceptTIO $ allocateUIConstants (coContents cfg)

              withDynamicBindings (fromMaybe (Vertical []) toolConfig) $ \innerContext -> do
                -- Build the event handler actions using the viewer context
                -- extended by the tool's configuration context.
                cvToolContext <- case innerContext <#> inheritedContext of
                  Left msg -> throwIO (BadProject "there was a problem parsing the tool's configuration" msg)
                  Right ctx -> pure ctx
                putStrLn ("building tool `" ++ tiName ptoolInfo ++ "`")
                putStrLn ("environment: " ++ show (contextToEnv cvToolContext))
                (toolVars, h) <- case toEventHandlers
                                      (contextToEnv cvToolContext)
                                      splices ptoolEventHandlers of
                       Left err -> throwIO (BadProject "there was a problem parsing the tool event handler" err)
                       Right ok -> pure ok
                let toolInfo = ptoolInfo
                    toolDrawLayer = ptoolDrawLayer
                    toolRefreshOnActivate = ptoolRefreshOnActivate
                    toolEventHandler = const Nothing -- to be replaced below
                    tool = Tool{..}
                pure (tool { toolEventHandler = handleEvent cvToolContext (drawTo ptoolDrawLayer) h })

            action ViewerUIProperties{..} ComplexViewer'{..}

newtype ComplexViewerCompiler = ComplexViewerCompiler
  { withCompiledComplexViewer
    :: forall x y dx dy out env t
     . ( KnownEnvironment env
       , NotPresent "[internal argument] #blockWidth" env
       , NotPresent "[internal argument] #blockHeight" env
       , NotPresent "[internal argument] #subsamples" env
       , KnownSymbol x, KnownSymbol y
       , KnownSymbol dx, KnownSymbol dy
       , KnownSymbol out
       , Required x env ~ 'RealT
       , NotPresent x (env `Without` x)
       , Required y env ~ 'RealT
       , NotPresent y (env `Without` y)
       , Required dx env ~ 'RealT
       , NotPresent dx (env `Without` dx)
       , Required dy env ~ 'RealT
       , NotPresent dy (env `Without` dy)
       , Required out env ~ 'ColorT
       , NotPresent out (env `Without` out)
       )
    => Proxy x
    -> Proxy y
    -> Proxy dx
    -> Proxy dy
    -> Proxy out
    -> Code env
    -> ((Int32 -> Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
         -> IO t)
    -> IO t
  }


complexToReal :: forall env z px
               . ( KnownEnvironment env
                 , KnownSymbol z, KnownSymbol px
                 , NotPresent z env, NotPresent px ( '(z, 'ComplexT) ': env ) )
              => NameIsPresent InternalX 'RealT env
              -> NameIsPresent InternalY 'RealT env
              -> NameIsPresent InternalPx 'RealT ( '(z, 'ComplexT) ': env )
              -> Code ( '(px, 'RealT) ': '(z, 'ComplexT) ': env )
              -> Code env
complexToReal reZ imZ vpx =
  let z = AddC (R2C (Var Proxy RealType reZ))
               (MulC (Const (Scalar ComplexType (0 :+ 1)))
                     (R2C (Var Proxy RealType imZ)))
      px = Var Proxy RealType vpx
  in  let_ z
    . let_ px

consDrawCmd :: Draw_ value env
            -> Maybe [Draw_ value env]
            -> Maybe [Draw_ value env]
consDrawCmd = \case
  Clear{} -> const (Just [])
  c -> Just . \case
    Nothing -> [c]
    Just cs -> c:cs

makeDrawCommandGetter :: IO (IO [[DrawCommand]], IO Bool, Int -> DrawHandler ScalarIORefM)
makeDrawCommandGetter = do
  layersMVar <- newMVar (Map.empty :: Map Int [DrawCommand])
  dcChanged <- newMVar False
  let drawTo :: Int -> DrawHandler ScalarIORefM
      drawTo n = DrawHandler $ \cmd -> do
        let emit :: DrawCommand -> StateT (Context IORefTypeOfBinding e) IO ()
            emit cmd' = liftIO $ do
              modifyMVar_ dcChanged (pure . const True)
              modifyMVar_ layersMVar (pure . Map.alter (consDrawCmd cmd') n)
        case cmd of
          DrawPoint _env pv -> do
            p <- eval pv
            emit (DrawPoint EmptyEnvProxy p)
          DrawCircle _env doFill rv pv -> do
            r    <- eval rv
            p    <- eval pv
            emit (DrawCircle EmptyEnvProxy doFill r p)
          DrawLine _env fromv tov -> do
            from <- eval fromv
            to   <- eval tov
            emit (DrawLine EmptyEnvProxy from to)
          DrawRect _env doFill fromv tov -> do
            from <- eval fromv
            to   <- eval tov
            emit (DrawRect EmptyEnvProxy doFill from to)
          SetStroke _env cv -> do
            c <- eval cv
            emit (SetStroke EmptyEnvProxy c)
          SetFill _env cv -> do
            c <- eval cv
            emit (SetStroke EmptyEnvProxy c)
          Clear _env -> emit (Clear EmptyEnvProxy)

  let getDrawCommands = do
        tryTakeMVar dcChanged >>= \case
          Nothing -> pure ()
          Just _  -> putMVar dcChanged False
        tryReadMVar layersMVar >>= \case
          Nothing -> pure [[]]
          Just m  -> pure (map reverse (Map.elems m))

      drawCommandsChanged = tryReadMVar dcChanged >>= \case
        Nothing -> pure True
        Just tf -> pure tf
  pure (getDrawCommands, drawCommandsChanged, drawTo)

runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO action = runExceptT action >>= \case
  Right result -> pure result
  Left err     -> throwIO (BadProject "there was a problem building the viewer's configuration values." err)
