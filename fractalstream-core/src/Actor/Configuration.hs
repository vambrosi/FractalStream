{-# language OverloadedStrings #-}
module Actor.Configuration
  ( Configuration(..)
  , withConfigurationEnv
  , getConfigurationSplices
  ) where

import Actor.Layout
import Language.Type
import Language.Environment
import Language.Value.Parser
import qualified Data.Map as Map

import Data.Aeson

data Configuration = Configuration
  { coTitle :: String
  , coSize :: (Int, Int)
  , coContents :: Layout Dummy
  }
  deriving Show

instance FromJSON Configuration where
  parseJSON = withObject "configuration" $ \o -> do
    coTitle <- o .: "title"
    Dimensions coSize <- o .: "size"
    coContents <- parseLayout o
    pure Configuration{..}

withConfigurationEnv :: forall m t env
                           . MonadFail m
                          => Maybe Configuration
                          -> EnvironmentProxy env
                          -> (forall env'. EnvironmentProxy env' -> m t)
                          -> m t
withConfigurationEnv Nothing env0 k = k env0
withConfigurationEnv (Just Configuration{..}) env0 k
   = go (allBindings coContents) env0
 where
   go :: forall e. [(String, SomeType)] -> EnvironmentProxy e -> m t
   go [] env = k env
   go ((nameStr, SomeType ty) : etc) env =
     bindInEnv nameStr ty env (go etc)

getConfigurationSplices :: forall m. MonadFail m
                        => Configuration
                        -> m Splices
getConfigurationSplices Configuration{..}
    = (>>= traverse theOnly)
    . fmap (Map.fromListWith (<>) . map (\(k,v) -> (k, [v])))
    . mapM getSplice
    $ allBindingVars coContents
  where
    theOnly :: [a] -> m a
    theOnly = \case
      [x] -> pure x
      _   -> fail ("duplicate splices")

    getSplice :: ConfigVar -> m (String, TypedValue)
    getSplice (ConfigVar valStr (SomeType _ty) _envMap name) = do
      case parseTypedValue Map.empty valStr of
        Left e -> fail e
        Right v -> pure (name, v)
{-
withConfigurationSplices :: forall m t
                          . MonadFail m
                         => Maybe Configuration
                         -> (Splices -> m t)
                         -> m t
withConfigurationSplices Nothing k = k EmptyContext
withConfigurationSplices (Just Configuration{..}) k
   = go (allBindingVars coContents) EmptyEnvProxy EmptyContext
 where
   go :: forall e. [ConfigVar] -> EnvironmentProxy e -> Context Splice e ->  m t
   go [] _ ctx = k ctx
   go ((ConfigVar valStr (SomeType ty) envMap nameStr) : etc) ctxEnv ctx =
     case someSymbolVal nameStr of
       SomeSymbol name -> bindInEnv' name ty ctxEnv $ \ctxEnv' -> do
         let die e = fail $ "parse error when parsing config argument `" <>
               nameStr <> "`: " <> show (snd e)

         withEnvFromMap envMap $ \env ->
           -- see if we can parse this as a value at all
           case parseUntypedValue valStr of
             Left e -> die e
             Right v ->
               -- now see if we can parse at the right type,
               -- in the expected environment
               case parseValue env EmptyContext ty valStr of
                 Left e -> die e
                 Right _ -> go etc ctxEnv' (Bind name ty v ctx)
-}
