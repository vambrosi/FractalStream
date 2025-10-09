{-# language OverloadedStrings, RecursiveDo #-}
module Language.Effect.Output
  ( Output(..)
  , outputEffectParser
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Effect
import Language.Parser
import Language.Value.Parser
import Data.Indexed.Functor

-- | An 'Output' effect introduces a set of typed write-only variables.
data Output (outputs :: Environment) (code :: Environment -> Exp Type) (env :: Environment) where
  Output :: forall name ty outputs env code
          . (KnownSymbol name, KnownType ty)
         => EnvironmentProxy env
         -> NameIsPresent name ty outputs
         -> Proxy name
         -> Value '(env, ty)
         -> Output outputs code env

instance IFunctor (Output outputs) where
  type IndexProxy (Output outputs) = EnvironmentProxy
  imap _ (Output env pf name v) = Output env pf name v
  toIndex (Output env _ _ _) = env

instance ITraversable (Output outputs) where
  isequence (Output env pf name v) = pure (Output env pf name v)

-- | output VALUE to NAME
outputEffectParser :: forall outputs
                    . EnvironmentProxy outputs
                   -> EffectParser (Output outputs)
outputEffectParser outputs = EffectParser (Proxy @(Output outputs)) $ \value _code -> mdo

  output <- rule $
    mkOutput outputs
      <$> (tok "output" *> value)
      <*> (tok "to" *> tokenMatch (\case { Identifier n -> Just n; _ -> Nothing })
            <* token Newline)

  pure output

mkOutput :: EnvironmentProxy outputs
         -> TypedValue
         -> String
         -> EnvCodeOfEffect (Output outputs) code
mkOutput outputEnv v n = EnvCodeOfEffect $ \env ->
  withEnvironment env $ do
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv' name outputEnv of
      Absent' _ -> Errs $ Left ["No output variable named " ++ n ++ " is in scope here."]
      Found' ty pf -> withKnownType ty $
        (Output env pf name <$> atType v ty)

tok :: String -> Prod r String Token ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  _ -> Nothing
