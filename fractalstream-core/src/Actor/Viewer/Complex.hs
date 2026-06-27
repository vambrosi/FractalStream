{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
  , PrepOutputSpec(..)
  , PrepRaw(..)
  , ContinuationRaw(..)
  , anchorFunction
--  , listenForChanges
  ) where

import FractalStream.Prelude

import Actor.Viewer
import Actor.Layout
import Actor.Tool
import Actor.Event

import Language.Type
import Language.Code
import Data.DynamicValue
import Data.Codec

import Language.Value.Parser
import Language.Value.Typecheck
  (InternalVanishingRadius, InternalEscapeRadius,
   InternalIterations, InternalStuck, InternalIterationLimit)
import Language.Typecheck
import Language.Parser.SourceRange
import Language.Code.Parser

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.:?), (.!=), object, (.=))
import qualified Data.Map as Map

-- | Raw (pre-type-checked) description of one prep script output variable.
data PrepOutputSpec = PrepOutputSpec
  { posVariable :: String
  , posType     :: String
  , posDefault  :: String
  } deriving (Eq, Show)

instance FromJSON PrepOutputSpec where
  parseJSON = withObject "prep output" $ \o ->
    PrepOutputSpec <$> o .: "variable" <*> o .: "type" <*> o .: "default"

instance ToJSON PrepOutputSpec where
  toJSON PrepOutputSpec{..} = object
    [ "variable" .= posVariable
    , "type"     .= posType
    , "default"  .= posDefault
    ]

-- | Raw (pre-type-checked) preparation script: output declarations plus code.
data PrepRaw = PrepRaw
  { prOutputs :: [PrepOutputSpec]
  , prCode    :: String
  } deriving (Eq, Show)

instance FromJSON PrepRaw where
  parseJSON = withObject "preparation" $ \o ->
    PrepRaw <$> o .: "outputs" <*> o .: "code"

instance ToJSON PrepRaw where
  toJSON PrepRaw{..} = object
    [ "outputs" .= prOutputs
    , "code"    .= prCode
    ]

instance Codec PrepRaw where codec = aeson

-- | Raw (pre-type-checked) continuation script. Like 'PrepRaw' (reuses the same
-- 'PrepOutputSpec' output declarations and a code block), but additionally names
-- the continued unknown and the first-point anchor. The engine seeds 'crUnknown'
-- from the previous point's published solution as it walks the tile; 'crAnchor'
-- supplies the unknown's initial guess at the very first point only.
data ContinuationRaw = ContinuationRaw
  { crUnknown :: String           -- ^ the continued variable
  , crAnchor  :: String           -- ^ initial guess for the unknown at point 0
  , crOutputs :: [PrepOutputSpec]
  , crCode    :: String
  , crDownsample :: Int           -- ^ field-grid spacing in pixels per axis: the
                                  --   pass solves one point per @d x d@ block and
                                  --   the viewer reads the nearest one (d=1 is full
                                  --   resolution). A coarser grid is much faster but
                                  --   blockier; default 16 (one solve per 16x16 block).
  } deriving (Eq, Show)

instance FromJSON ContinuationRaw where
  parseJSON = withObject "continuation" $ \o ->
    ContinuationRaw <$> o .: "unknown" <*> o .: "anchor"
                    <*> o .: "outputs" <*> o .: "code"
                    <*> o .:? "downsampling-factor" .!= 16

instance ToJSON ContinuationRaw where
  toJSON ContinuationRaw{..} = object
    [ "unknown" .= crUnknown
    , "anchor"  .= crAnchor
    , "outputs" .= crOutputs
    , "code"    .= crCode
    , "downsampling-factor" .= crDownsample
    ]

instance Codec ContinuationRaw where codec = aeson

-- | Existential packaging of both 'prepOutputEnv' and 'combinedEnv' so that
-- both type variables are simultaneously in scope when constructing
-- 'PrepScript' and 'SomeViewerWithContext'.
data MergedPrep where
  MergedPrep :: ( KnownEnvironment prepOutputEnv
                , KnownEnvironment combinedEnv )
             => EnvironmentProxy prepOutputEnv    -- ^ identifies prep output vars
             -> Context DynamicValue combinedEnv  -- ^ config ++ prep output vars
             -> Maybe String                      -- ^ prep code string if present
             -> MergedPrep

-- | Build a 'SomeContext DynamicValue' from a list of prep output specs.
-- Each output gets a constant 'Dynamic' holding its parsed default value.
buildPrepCtxFromSpecs :: [PrepOutputSpec] -> Either String (SomeContext DynamicValue)
buildPrepCtxFromSpecs specs =
  case mconcat (map buildOne specs) of
    SomeContext' (Left err)  -> Left err
    SomeContext' (Right sc)  -> Right sc
  where
    buildOne :: PrepOutputSpec -> SomeContext' DynamicValue
    buildOne PrepOutputSpec{..} = case parseType' posType of
      Left err -> SomeContext' (Left err)
      Right (SomeType ty) -> withKnownType ty $ case parseConstant' ty posDefault of
        Left err -> SomeContext' (Left err)
        Right defVal ->
          case someSymbolVal posVariable of
            SomeSymbol name -> SomeContext' (Right (SomeContext (Bind name ty (pure defVal) EmptyContext)))

-- | Merge a config context with prep output variables extracted from 'PrepRaw'.
-- Returns a 'MergedPrep' that exposes both 'prepOutputEnv' and 'combinedEnv'.
buildMergedPrep :: forall configEnv
                 . KnownEnvironment configEnv
                => Context DynamicValue configEnv
                -> Maybe PrepRaw
                -> Either String MergedPrep
buildMergedPrep configCtx Nothing =
  Right (MergedPrep EmptyEnvProxy configCtx Nothing)
buildMergedPrep configCtx (Just (PrepRaw outputs codeStr)) =
  case buildPrepCtxFromSpecs outputs of
    Left err -> Left err
    Right (SomeContext (prepCtx :: Context DynamicValue prepOutputEnv)) ->
      case configCtx <#> prepCtx of
        Left err -> Left err
        Right combinedCtx ->
          withEnvironment (contextToEnv combinedCtx) $
            Right (MergedPrep (contextToEnv prepCtx) combinedCtx (Just codeStr))

-- | Like 'MergedPrep', but also folds in the continuation outputs and the
-- continued unknown. 'combinedEnv' is config ++ prep outputs ++ continuation
-- outputs ++ {unknown}; the body and the continuation code are both parsed in it.
-- Exposes the prep output env, the continuation output env (the field arrays),
-- and — when a continuation block is present — the unknown name, parsed anchor,
-- and continuation code string.
data MergedViewer where
  MergedViewer :: ( KnownEnvironment prepOutputEnv
                  , KnownEnvironment contOutputEnv
                  , KnownEnvironment combinedEnv )
               => EnvironmentProxy prepOutputEnv
               -> EnvironmentProxy contOutputEnv
               -> Context DynamicValue combinedEnv
               -> Maybe String                            -- ^ prep code string
               -> Maybe (String, Complex Double -> Complex Double, Int, String)
                  -- ^ (unknown, anchor-as-function-of-coord, downsampling factor, cont code)
               -> MergedViewer

-- | The continuation anchor: either the coordinate itself (@anchor: c@, becomes
-- @id@) or a constant expression (becomes @const k@). @coordName@ is the viewer's
-- z-coordinate variable name. Fuller anchor expressions (e.g. @c/2@) are future.
anchorFunction :: String -> String -> Either String (Complex Double -> Complex Double)
anchorFunction coordName anchorStr
  | unwords (words anchorStr) == unwords (words coordName) = Right id
  | otherwise = case parseConstant' ComplexType anchorStr of
      Left err -> Left ("continuation anchor: " ++ err)
      Right k  -> Right (const k)

buildMergedViewer :: forall configEnv
                   . KnownEnvironment configEnv
                  => Context DynamicValue configEnv
                  -> Maybe PrepRaw
                  -> Maybe ContinuationRaw
                  -> String                 -- ^ z-coordinate variable name
                  -> Either String MergedViewer
buildMergedViewer configCtx mPrepRaw mContRaw coordName =
  case buildMergedPrep configCtx mPrepRaw of
    Left err -> Left err
    Right (MergedPrep prepEnvProxy ctx1 mPrepCode) -> case mContRaw of
      Nothing ->
        Right (MergedViewer prepEnvProxy EmptyEnvProxy ctx1 mPrepCode Nothing)
      Just (ContinuationRaw unknown anchorStr contOutputs contCode downsample) ->
        case anchorFunction coordName anchorStr of
          Left err -> Left err
          Right anchorVal -> case buildPrepCtxFromSpecs contOutputs of
            Left err -> Left err
            Right (SomeContext contCtx) ->
              case buildPrepCtxFromSpecs [PrepOutputSpec unknown "C" "0"] of
                Left err -> Left err
                Right (SomeContext unkCtx) -> case ctx1 <#> contCtx of
                  Left err -> Left err
                  Right ctx2 -> case ctx2 <#> unkCtx of
                    Left err -> Left err
                    Right ctx3 ->
                      withEnvironment (contextToEnv contCtx) $
                        withEnvironment (contextToEnv ctx3) $
                          Right (MergedViewer prepEnvProxy (contextToEnv contCtx)
                                   ctx3 mPrepCode (Just (unknown, anchorVal, downsample, contCode)))

data ComplexViewer = ComplexViewer
  { cvTitle :: Parsed String
  , cvSize :: Variable Dimensions
  , cvPosition :: Variable Dimensions
  , cvCanResize :: Variable Bool
  , cvCenter :: Parsed (Complex Double)
  , cvPixelSize :: Parsed Double
  , cvCoord :: Parsed String
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  , cvEscapeRadius :: Parsed (Maybe ParsedValue)
  , cvVanishRadius :: Parsed (Maybe ParsedValue)
  , cvIterationLimit :: Parsed (Maybe ParsedValue)
  , cvPrep :: Variable (Maybe PrepRaw)
  , cvContinuation :: Variable (Maybe ContinuationRaw)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerWithContext)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }

instance CodecWith ScriptDependencies ComplexViewer where
  codecWith_ ctx = do
    title  <-cvTitle-< mapped (key "title") $ \_ -> pure nonEmptyString
    size   <-cvSize-< key "size"
    pos    <-cvPosition-<  keyWithDefaultValue (Dimensions (100, 100)) "position"
    resize <-cvCanResize-< keyWithDefaultValue True "resizable"
    center <-cvCenter-< mapped (keyWithDefaultValue "0" "initial-center") $ \_ ->
      pure (parseConstant' ComplexType)
    pxSize <-cvPixelSize-< mapped (keyWithDefaultValue "1/128" "initial-pixel-size") $ \_ ->
      pure (parseConstant' RealType)
    coord  <-cvCoord-< mapped (key "z-coord") $ \_ -> pure nonEmptyString
    pixel  <-cvPixel-< mapped (keyWithDefaultValue Nothing "pixel-size") $ \_ ->
      pure (traverse nonEmptyString)

    esc  <-cvEscapeRadius-<   mapped (optionalKey "escape-radius")    $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s
    van  <-cvVanishRadius-<   mapped (optionalKey "vanishing-radius") $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s
    iter <-cvIterationLimit-< mapped (optionalKey "iteration-limit")  $ \_ -> pure $ \case
      "" -> pure Nothing
      s  -> fmap Just . left (`ppFullError` s) . parseParsedValue Map.empty $ s

    prep   <-cvPrep-< keyWithDefaultValue Nothing "preparation"
    cont   <-cvContinuation-< keyWithDefaultValue Nothing "continuation"

    code   <-cvCode-< mapped (key "code") $ \use -> do
      let complain err = pure . const . Left . (NoSourceRange,)
                    $ ("Cannot parse the script because of previous errors: " ++ err)
          (dcontext, dsplices) = use ctx
      dsplices >>= \case
        Left err -> complain err
        Right vcSplices -> dcontext >>= \case
          Left err -> complain err
          Right (SomeContext (configCtx :: Context DynamicValue configEnv)) -> do
            mPrepRaw <- dyn (use prep)
            mContRaw <- dyn (use cont)
            -- z-coord name, for resolving `anchor: c` (coordinate-relative). If
            -- the coord errored it surfaces later via vcCoord; "" is a safe filler.
            coordName <- either (const "") id <$> dyn (use coord)
            case buildMergedViewer configCtx mPrepRaw mContRaw coordName of
              Left err -> complain err
              Right (MergedViewer (prepEnvProxy :: EnvironmentProxy prepOutputEnv)
                                  (contOutputEnvProxy :: EnvironmentProxy contOutputEnv)
                                  (combinedCtx :: Context DynamicValue combinedEnv)
                                  mPrepCodeStr mContInfo) -> do
                let env = contextToEnv combinedCtx
                    vcContext = combinedCtx
                    assertAbsentViewerArgs :: forall e t
                                           . EnvironmentProxy e
                                          -> (MissingViewerArgs e => Either String t)
                                          -> Either String t
                    assertAbsentViewerArgs e action =
                      fromMaybe (Left "Internal error") (assertMissingViewerArgs e action)

                vc :: Either String (ViewerContext combinedEnv) <- withEnvironment env $
                  (fromMaybe (pure $ Left "Internal error") . assertMissingViewerArgs env) $
                  runExceptT $ do
                  vcCoord <- ExceptT (dyn $ use coord)

                  vcIterLimit <- ExceptT $ (dyn $ use iter) <&> \case
                    Left err -> Left err
                    Right Nothing -> Right Nothing
                    Right (Just pv) -> assertAbsentViewerArgs env $ do
                      case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
                        Absent pf -> recallIsAbsent pf $
                          case lookupEnv (Proxy @InternalEscapeRadius) RealType env of
                            Absent pf' -> recallIsAbsent pf' $ case pv `atType` IntegerType of
                              TC (Left err) -> Left ("Error with internal iteration limit: " ++ ppError err)
                              TC (Right x)  -> pure (Just x)
                            _ -> Left "Internal error"
                        _ -> Left "Internal error"

                  vcEscapes <- ExceptT $ (dyn $ use esc) <&> \case
                    Left err -> Left err
                    Right Nothing -> Right Nothing
                    Right (Just pv) -> assertAbsentViewerArgs env $ do
                      case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
                        Absent pf -> recallIsAbsent pf $ case pv `atType` RealType of
                          TC (Left err) -> Left ("Error with internal escape radius: " ++ ppError err)
                          TC (Right x)  -> pure (Just x)
                        _ -> Left "Internal error"

                  vcVanishes <- ExceptT $ (dyn $ use van) <&> \case
                    Left err -> Left err
                    Right Nothing -> Right Nothing
                    Right (Just pv) -> assertAbsentViewerArgs env $ do
                      case pv `atType` RealType of
                        TC (Left err) -> Left ("Error with internal vanishing radius: " ++ ppError err)
                        TC (Right x)  -> pure (Just x)

                  pure ViewerContext{..}
                case vc of
                  Left err   -> complain err
                  Right args -> dyn (use pixel) >>= \case
                    Left err -> complain err
                    Right mpx ->
                      case assertMissingViewerArgs (envProxy (Proxy @combinedEnv)) $
                           \(CodeString viewerSrc) -> do
                             mPrepScript <- case mPrepCodeStr of
                               Nothing -> pure Nothing
                               Just prepSrc -> do
                                 let declareE :: forall n -> forall t e. KnownSymbol n
                                              => TypeProxy t -> EnvironmentProxy e
                                              -> Either (SourceRange, String) (EnvironmentProxy ('(n,t) ': e))
                                     declareE n t e = case lookupEnv' (Proxy @n) e of
                                       Found'{} -> throwError (NoSourceRange,
                                                     "Internal error: duplicate `" ++ symbolVal (Proxy @n) ++ "`")
                                       Absent' pf -> pure (recallIsAbsent pf $ declare t e)
                                 env' <- (     declareE InternalIterations IntegerType
                                           <=< declareE InternalStuck BooleanType
                                           <=< declareE InternalIterationLimit IntegerType
                                           <=< declareE InternalEscapeRadius RealType
                                           <=< declareE InternalVanishingRadius RealType
                                           <=< declareE InternalX RealType
                                           <=< declareE InternalY RealType
                                           <=< declareE InternalDX RealType
                                           <=< declareE InternalDY RealType
                                           <=< declareE "color" ColorType
                                         ) env
                                 prepCode0 <- withEnvironment env' $
                                   left (errorLocation &&& unlines . pp) (parseCode env' vcSplices prepSrc)
                                 -- Wrap the bookkeeping variables via letInEnv so the prep
                                 -- code has type Code (ViewerEnv env), matching the viewer code.
                                 let (_, prepCode) = (env', prepCode0)
                                       & letInEnv (Const (Scalar typeProxy 0))
                                       & letInEnv (Const (Scalar typeProxy False))
                                       & letInEnv (fromMaybe (Const (Scalar typeProxy 100)) (vcIterLimit args))
                                       & letInEnv (fromMaybe (Const (Scalar typeProxy 10.0)) (vcEscapes args))
                                       & letInEnv (fromMaybe (Const (Scalar typeProxy 0.0001)) (vcVanishes args))
                                 pure (Just (PrepScript prepEnvProxy prepCode))
                             viewerCode <- parseViewerScript mpx args (CodeString viewerSrc)
                             -- The continuation code is coordinate-aware and uses
                             -- `solve` exactly like a viewer body, so it parses with
                             -- the same machinery in the same (merged) env.
                             mContScript <- case mContInfo of
                               Nothing -> pure Nothing
                               Just (unknownName, anchorVal, downsample, contSrc) -> do
                                 contCode <- parseViewerScript mpx args (CodeString contSrc)
                                 pure (Just (ContinuationScript contOutputEnvProxy
                                               unknownName anchorVal downsample contCode))
                             pure (SomeViewerWithContext combinedCtx mPrepScript mContScript viewerCode)
                      of
                        Nothing -> pure . const . Left . (NoSourceRange,) $ "INTERNAL ERROR: redefined internal argument"
                        Just fn -> pure fn

    ctx' <- purely $ \use -> ( snd (use ctx)
                             , fmap ComplexCoordinate <$> dyn (use coord)
                             , (\x y z -> (,,) <$> x <*> y <*> z)
                               <$> (fmap (fromMaybe defaultIterLimit) <$> (dyn $ use iter))
                               <*> (fmap (fromMaybe defaultMaxRadius) <$> (dyn $ use esc))
                               <*> (fmap (fromMaybe defaultMinRadius) <$> (dyn $ use van))
                             , dyn (use pixel))

    tools  <-cvTools-< optionalField "tools" (newVariable []) (fmap null . getDynamic) $ do
      codecWith ctx'
    build ComplexViewer title size pos resize center pxSize coord pixel esc van iter prep cont code tools
