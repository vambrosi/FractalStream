{-# language UndecidableInstances, OverloadedStrings #-}
module Actor.Layout
  ( Layout(..)
  , Label(..)
  , Dimensions(..)
  , allBindings
  , allBindingVars
  , extractAllBindings
  , Dummy(..)
  , ConfigVar(..)
  , StringOrNumber(..)
  -- *
  , parseLayout
  , parseToHaskellValue
  , setTextOnly
  , setToParsed
  , Expression(..)
  , ConstantExpression(..)
  , ConstantExpression'(..)
  , allocateUIExpressions
  , allocateUIConstants
  , withSplices
  , withDynamicBindings
  , toSomeDynamic
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Data.Color
import Data.DynamicValue
import Language.Value.Evaluator (HaskellTypeOfBinding, evaluate)
import Language.Value
import Language.Value.Parser
import Language.Code
import Language.Code.Parser
import Language.Value.Typecheck ( internalVanishingRadius
                                , internalEscapeRadius
                                , internalIterationLimit
                                , internalIterations
                                , internalStuck )
import Language.Parser.SourceRange
import Language.Typecheck


import Data.Aeson hiding (Value)
import qualified Data.Aeson.Types as JSON
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read (readMaybe)

newtype Label = Label String
  deriving (Show, IsString)

instance FromJSON Label where
  parseJSON = withText "label" (pure . Label . Text.unpack)

data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)
  | PlainText String
  | Button String
  | Multiline (f String)

deriving instance (Show (f String), Show (f Bool), Show (f Color)) => Show (Layout f)

data Dimensions = Dimensions (Int, Int)

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ \txt -> do
    case Text.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
          Just dim -> pure (Dimensions dim)
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"

data Dummy t = Dummy ConfigVar
  deriving Show

data ConfigVar = ConfigVar
  { varValue :: String
  , varType :: SomeType
  , varEnv :: Map String SomeType
  , varVariable :: String
  }
  deriving Show

instance FromJSON (Layout Dummy) where
  parseJSON = withObject "layout" parseLayout

newtype SomeType' = SomeType' { getSomeType :: SomeType }

instance FromJSON SomeType' where
  parseJSON = withText "type" $ \txt -> do
    case parseType (Text.unpack txt) of
      Left err -> fail (ppFullError err $ Text.unpack txt)
      Right t  -> pure (SomeType' t)

parseLayout :: Object -> JSON.Parser (Layout Dummy)
parseLayout o
  =   (Vertical <$> (o .: "vertical-contents"))
  <|> (Horizontal <$> (o .: "horizontal-contents"))
  <|> (uncurry Panel <$> (titled =<< (o .: "panel")))
  <|> (Tabbed <$> ((o .: "tabbed") >>= mapM titled))
  <|> (textBoxLayout =<< (o .: "text-entry"))
  <|> (checkBoxLayout =<< (o .: "checkbox"))
  <|> (colorPickerLayout =<< (o .: "color-picker"))
  <|> (PlainText <$> (o .: "text"))
  <|> (Button <$> (o .: "button"))
  <|> (multilineLayout =<< (o .: "code"))
  <|> fail "bad layout description"
 where
   titled p = (,) <$> (p .: "title") <*> parseLayout p

   textBoxLayout p = do
     lab <- p .: "label"
     StringOrNumber varValue <- p .: "value"
     SomeType' varType <- p .: "type"
     varVariable <- p .: "variable"
     varEnv <- Map.map getSomeType <$> (p .:? "environment" .!= Map.empty)
     pure (TextBox lab (Dummy ConfigVar{..}))

   checkBoxLayout p = do
     lab <- p .: "label"
     val <- p .: "value"
     let varValue = if val then "true" else "false"
         varType = SomeType BooleanType
         varEnv = Map.empty
     varVariable <- p .: "variable"
     pure (CheckBox lab (Dummy ConfigVar{..}))

   colorPickerLayout p = do
     lab <- p .: "label"
     varValue <- p .: "value"
     varVariable <- p .: "variable"
     let varType = SomeType ColorType
         varEnv = Map.empty
     pure (ColorPicker lab (Dummy ConfigVar{..}))

   multilineLayout p = do
     varVariable <- p .: "variable"
     varEnv <- Map.map getSomeType <$> (p .:? "environment" .!= Map.empty)
     let varType = SomeType TextType
     varValue <- p .: "value"
     pure (Multiline (Dummy ConfigVar{..}))

allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = map (\ConfigVar{..} -> (varVariable, varType)) . allBindingVars

allBindingVars :: Layout Dummy -> [ConfigVar]
allBindingVars = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ (Dummy x) -> [x]
      CheckBox _ (Dummy x) -> [x]
      ColorPicker _ (Dummy x) -> [x]
      PlainText _ -> []
      Button {} -> []
      Multiline (Dummy x) -> [x]

extractAllBindings :: (forall t. f t -> a)
                   -> Layout f
                   -> [a]
extractAllBindings extractor = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ x -> [extractor x]
      CheckBox _ x -> [extractor x]
      ColorPicker _ x -> [extractor x]
      PlainText _ -> []
      Button {} -> []
      Multiline x -> [extractor x]

parseToHaskellValue :: forall env ty
                     . Context HaskellTypeOfBinding env
                    -> TypeProxy ty
                    -> String
                    -> Either String (HaskellType ty)
parseToHaskellValue ctx ty input =
  withKnownType ty $
  withEnvironment (contextToEnv ctx) $
  case parseInputValue @env @ty Map.empty input of
    Left err -> Left (ppFullError err input)
    Right v  -> pure (evaluate v ctx)

setTextOnly :: forall ty
             . TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setTextOnly _ s ui = do
  modifyUIValue ui (\(_, v) -> (s, v))
  pure Nothing

setToParsed :: forall ty
             . (TypeProxy ty -> String -> Either (Either ParseError TCError) (HaskellType ty))
            -> TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setToParsed parser ty input ui = do
  case parser ty input of
    Left err -> pure (Just $ ppFullError err input)
    Right v  -> setUIValue ui (input, v) >> pure Nothing


data Expression t where
  Expression :: forall env ty
              . String
             -> EnvironmentProxy env
             -> TypeProxy ty
             -> UIValue (String, Value '(env, ty))
             -> Expression String
  BoolExpression :: String -> UIValue Bool -> Expression Bool
  ColorExpression :: String -> UIValue Color -> Expression Color
  ScriptExpression :: forall env
                       . String
                      -> EnvironmentProxy env
                      -> UIValue (String, Code env)
                      -> Expression String

instance Dynamic Expression where
  getDynamic = \case
    BoolExpression _ b -> getDynamic b
    ColorExpression _ c -> getDynamic c
    Expression _ _ _ v -> fst <$> getDynamic v
    ScriptExpression _ _ e -> do
      x <- fst <$> getDynamic e
      putStrLn ("Script expression:\n" ++ x)
      return x

  setDynamic d new = case d of
    BoolExpression _ b -> setDynamic b new
    ColorExpression _ c -> setDynamic c new
    ScriptExpression _ env e ->
      withEnvironment env $ do
        case parseCode env basicSplices new of
          Left err -> pure (Just $ ppFullError err new)
          Right newC -> do
            setDynamic e (new, newC)
            pure Nothing
    Expression _ (env :: EnvironmentProxy env) (ty :: TypeProxy ty) v ->
      withEnvironment env $ withKnownType ty $ do
        let new' = case ty of
              TextType -> show new
              _  -> new
        case parseInputValue @env @ty basicSplices new' of
          Left err   -> pure (Just $ ppFullError err new)
          Right newV -> do
            setDynamic v (new, newV)
            pure Nothing

  listenWith d action = case d of
    BoolExpression _ b -> listenWith b action
    ColorExpression _ c -> listenWith c action
    ScriptExpression _ _ e -> listenWith e (\old new -> action (fst old) (fst new))
    Expression _ _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

data ConstantExpression t where
  ConstantExpression :: forall ty
                      . String
                     -> TypeProxy ty
                     -> UIValue (String, HaskellType ty)
                     -> ConstantExpression String
  ConstantBoolExpression :: String -> UIValue Bool -> ConstantExpression Bool
  ConstantColorExpression :: String -> UIValue Color -> ConstantExpression Color
  ConstantScriptExpression :: String -> UIValue String -> ConstantExpression String

instance Dynamic ConstantExpression where
  getDynamic = \case
    ConstantBoolExpression _ b -> getDynamic b
    ConstantColorExpression _ c -> getDynamic c
    ConstantScriptExpression _ e -> getDynamic e
    ConstantExpression _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    ConstantBoolExpression _ b -> setDynamic b new
    ConstantColorExpression _ c -> setDynamic c new
    ConstantScriptExpression _ e -> setDynamic e new
    ConstantExpression _ (ty :: TypeProxy ty) v ->
      withKnownType ty $ do
      let new' = case ty of
                   TextType -> show new
                   _  -> new
      case parseInputValue @'[] @ty Map.empty new' of
        Left err   -> pure (Just $ ppFullError err new)
        Right newV -> do
          setDynamic v (new, evaluate newV EmptyContext)
          pure Nothing

  listenWith d action = case d of
    ConstantBoolExpression _ b -> listenWith b action
    ConstantColorExpression _ c -> listenWith c action
    ConstantScriptExpression _ e -> listenWith e action
    ConstantExpression _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

allocateUIExpressions :: Layout Dummy
                      -> ExceptT String IO (Layout Expression)
allocateUIExpressions = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy ConfigVar{..}) -> case varType of
        SomeType (ty :: TypeProxy ty) -> withKnownType ty $
          withEnvFromMap varEnv $ \(env :: EnvironmentProxy env) ->
            withEnvironment env $
            case parseInputValue @env @ty basicSplices varValue of
              Left err -> throwError (ppFullError err varValue)
              Right v  ->
                TextBox lab . Expression varVariable env ty
                <$> newUIValue (varValue, v)

      CheckBox lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'BooleanT basicSplices varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> CheckBox lab . BoolExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'ColorT basicSplices varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> ColorPicker lab . ColorExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      PlainText txt -> pure (PlainText txt)

      Button txt -> pure (Button txt)

      Multiline (Dummy ConfigVar{..}) ->
        withEnvFromMap varEnv $ \env -> withEnvironment env $
          case parseCode env basicSplices varValue of
            Left err -> throwError (ppFullError err varValue)
            Right c -> Multiline . ScriptExpression varVariable env
                       <$> newUIValue (varValue, c)

allocateUIConstants :: Layout Dummy
                    -> ExceptT String IO (Layout ConstantExpression)
allocateUIConstants = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy ConfigVar{..}) -> case varType of
        SomeType (ty :: TypeProxy ty) -> withKnownType ty $
          case parseInputValue @'[] @ty Map.empty varValue of
            Left err -> throwError (ppFullError err varValue)
            Right v  -> TextBox lab . ConstantExpression varVariable ty
                        <$> newUIValue (varValue, evaluate v EmptyContext)

      CheckBox lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'BooleanT Map.empty varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> CheckBox lab . ConstantBoolExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'ColorT Map.empty varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> ColorPicker lab . ConstantColorExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      PlainText txt -> pure (PlainText txt)

      Button txt -> pure (Button txt)

      Multiline _ -> error "TODO: allocateUIConstants for Multiline"

withSplices :: forall t
             . Layout Expression
            -> (Splices -> IO t) --(forall splices. Context Splice splices -> IO t)
            -> IO t
withSplices lo action =
    go [] (catMaybes $ extractAllBindings toSomeUIExpr lo)
  where
    go :: [(String, ParsedValue)] -> [SomeUIExpr] -> IO t
    go splices = \case
      [] -> action (Map.fromList splices)
      (SomeUIExpr name _ty getExpr : etc) -> do
        e <- getExpr
        go ((symbolVal name, e) : splices) etc

    unsafeFromRight :: Either a b -> b
    unsafeFromRight = \case
      Left{} -> error "Left"
      Right x -> x

    toSomeUIExpr :: forall a. Expression a -> Maybe SomeUIExpr
    toSomeUIExpr = \case
      Expression nameStr _ ty v -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ty (unsafeFromRight . parseParsedValue Map.empty . fst
                                <$> getDynamic v)
      BoolExpression nameStr b -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name BooleanType
            (unsafeFromRight . parseParsedValue Map.empty . showValue BooleanType <$>
             getDynamic b)
      ColorExpression nameStr b -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ColorType
            (unsafeFromRight . parseParsedValue Map.empty . showValue ColorType <$>
             getDynamic b)
      ScriptExpression {} -> Nothing -- can't build a splice value from a script

{-
withSplices lo action = go EmptyContext (extractAllBindings toSomeUIExpr lo)
  where
    go :: forall splices. Context Splice splices -> [SomeUIExpr] -> IO t
    go ctx = \case
      [] -> action ctx
      (SomeUIExpr name ty getExpr : etc) ->
        case lookupEnv name ty (contextToEnv ctx) of
          Absent proof -> recallIsAbsent proof $ do
            expr <- getExpr
            go (Bind name ty expr ctx) etc
          _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    pValue :: String -> U.Value
    pValue input = case parseUntypedValue input of
      Left err -> error (show err)
      Right v  -> v

    toSomeUIExpr :: forall a. Expression a -> SomeUIExpr
    toSomeUIExpr = \case
      Expression nameStr _ ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ty (pValue . fst <$> getDynamic v)
      BoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name BooleanType (pValue . showValue BooleanType <$> getDynamic b)
      ColorExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ColorType (pValue . showValue ColorType <$> getDynamic b)
-}

withDynamicBindings :: forall t
                     . Layout ConstantExpression
                    -> (forall env. Context DynamicValue env -> t)
                    -> t
withDynamicBindings lo action =
    go EmptyContext (extractAllBindings toSomeUIValue lo)
  where
    go :: forall env. Context DynamicValue env -> [SomeUIValue] -> t
    go ctx [] = action ctx
    go ctx ( (SomeUIValue name ty dyn) : xs ) =
      case lookupEnv name ty (contextToEnv ctx) of
        Absent proof -> recallIsAbsent proof
                        $ go (Bind name ty dyn ctx) xs
        _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    toSomeUIValue :: forall a. ConstantExpression a -> SomeUIValue
    toSomeUIValue = \case
      ConstantExpression nameStr ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ty (SomeDynamic (ConstantExpression' ty v))
      ConstantBoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name BooleanType (SomeDynamic b)
      ConstantColorExpression nameStr c ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ColorType (SomeDynamic c)
      ConstantScriptExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name TextType (SomeDynamic b)

data ConstantExpression' t where
  ConstantExpression' :: forall ty t
                      . (t ~ HaskellType ty)
                     => TypeProxy ty
                     -> UIValue (String, t)
                     -> ConstantExpression' t

instance Dynamic ConstantExpression' where
  getDynamic (ConstantExpression' _ d) =
    snd <$> getDynamic d

  setDynamic (ConstantExpression' ty d) v =
    setDynamic d (showValue ty v, v)

  listenWith (ConstantExpression' _ d) action =
    listenWith d (\(_, old) (_, new) -> action old new)

newtype StringOrNumber t = StringOrNumber { unStringOrNumber :: t }

instance (IsString s) => FromJSON (StringOrNumber s) where
  parseJSON v
    =   (withText "string" (pure . StringOrNumber . fromString . Text.unpack) v)
    <|> (withScientific "number" (pure . StringOrNumber . fromString . show) v)

toSomeDynamic :: Dynamic dyn => Layout dyn -> Layout SomeDynamic
toSomeDynamic = \case
  Vertical xs -> Vertical (map toSomeDynamic xs)
  Horizontal xs -> Horizontal (map toSomeDynamic xs)
  Panel lab x -> Panel lab (toSomeDynamic x)
  Tabbed ts -> Tabbed (map (\(lab, x) -> (lab, toSomeDynamic x)) ts)
  TextBox lab x -> TextBox lab (SomeDynamic x)
  CheckBox lab x -> CheckBox lab (SomeDynamic x)
  ColorPicker lab x -> ColorPicker lab (SomeDynamic x)
  PlainText txt -> PlainText txt
  Button txt -> Button txt
  Multiline txt -> Multiline (SomeDynamic txt)

basicSplices :: Splices
basicSplices = Map.fromList
  [ (internalEscapeRadius, ParsedValue NoSourceRange $ \case
        RealType -> pure 10.0
        _ -> throwError $ Advice NoSourceRange "The escape radius should be a real number"
    )
  , (internalVanishingRadius, ParsedValue NoSourceRange $ \case
        RealType -> pure 0.0001
        _ -> throwError $ Advice NoSourceRange "The minimum radius should be a real number"
    )
  , (internalIterationLimit, ParsedValue NoSourceRange $ \case
        IntegerType -> pure 100
        _ -> throwError $ Advice NoSourceRange "The iteration limit should be an integer"
    )
  , (internalIterations, ParsedValue NoSourceRange $ \case
        IntegerType -> pure 100
        _ -> throwError $ Advice NoSourceRange "The iteration count should be an integer"
    )
  , (internalStuck, ParsedValue NoSourceRange $ \case
        BooleanType -> pure $ Const (Scalar BooleanType True)
        _ -> throwError $ Advice NoSourceRange "`stuck` should be a truth value"
    )
  ]
