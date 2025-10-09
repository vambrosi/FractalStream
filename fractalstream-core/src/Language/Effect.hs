{-# language UndecidableInstances #-}

module Language.Effect
  ( type Effect
  , HasEffect(..)
  , EffectHandler(..)
  , Handlers(..)
  , type NoEffects
  , EffectParser(..)
  , EffectParsers(..)
  , EffectParsers_(..)
  , EnvCodeOf(..)
  , EnvCodeOfEffect(..)
  , noParser
  , mapHandlers
  ) where

import FractalStream.Prelude

import Language.Environment
import Language.Parser
import Language.Value.Parser (TypedValue)
import Data.Indexed.Functor

-- | An @Effect@ takes a family of nested code types, and produces
-- an AST of effects that may include nested code. Effects are a
-- kind of "@Code@ mix-in", so they should have the same kind
-- signature as @Code effects@.
type Effect = (Environment -> Exp Type) -> Environment -> Type

type NoEffects = '[] :: [Effect]

data EffectHandler (eff :: Effect) (result :: Environment -> Exp Type) where
  Handle :: forall result eff
          . Proxy result
         -> (forall env
              . EnvironmentProxy env
             -> eff result env
             -> Eval (result env))
         -> EffectHandler eff result

data Handlers (effs :: [Effect]) (result :: Environment -> Exp Type) where
  Handler :: forall eff effs result
           . (IFunctor eff, IndexProxy eff ~ EnvironmentProxy)
          => EffectHandler eff result
          -> Handlers effs result
          -> Handlers (eff ': effs) result
  NoHandler :: forall result. Handlers '[] result

mapHandlers :: forall effs a b
             . (forall env
                 . EnvironmentProxy env
                -> Eval (a env)
                -> Eval (b env))
            -> (forall env
                 . EnvironmentProxy env
                -> Eval (b env)
                -> Eval (a env))
            -> Handlers effs a
            -> Handlers effs b
mapHandlers f unF = \case
  NoHandler -> NoHandler
  Handler (Handle _ h) hs ->
    Handler (Handle (Proxy @b) (\e x -> f e (h e (imap unF x)))) (mapHandlers @_ @a @b f unF hs)


class (IndexProxy e ~ EnvironmentProxy, ITraversable e) => HasEffect (e :: Effect) (es :: [Effect]) where
  getHandler :: forall result
              . Proxy e
             -> Handlers es result
             -> EffectHandler e result

instance {-# OVERLAPS #-} (IndexProxy e ~ EnvironmentProxy, ITraversable e) => HasEffect e (e ': es) where
  getHandler _ (Handler h _) = h

instance {-# OVERLAPPABLE #-} HasEffect e es => HasEffect e (e' ': es) where
  getHandler e (Handler _ hs) = getHandler e hs

data EffectParser (eff :: Effect) where
  EffectParser :: forall eff
                . Proxy eff
               -> (forall code s
                   . Prod s String Token TypedValue
                  -> Prod s String Token (EnvCodeOf code)
                  -> Grammar s (Prod s String Token (EnvCodeOfEffect eff code)))
               -> EffectParser eff

newtype EffectParsers effs = EP (EffectParsers_ effs effs)

data EnvCodeOf (code :: Environment -> Exp Type) where
  EnvCodeOf :: forall code
             . (forall env. EnvironmentProxy env -> Errs (Eval (code env)))
            -> EnvCodeOf code

data EnvCodeOfEffect (eff :: (Environment -> Exp Type) -> Environment -> Type) (code :: Environment -> Exp Type) where
  EnvCodeOfEffect :: forall eff code
                   . (forall env. EnvironmentProxy env -> Errs (eff code env))
                  -> EnvCodeOfEffect eff code

data EffectParsers_ (effs :: [Effect]) (effs0 :: [Effect]) where
  ParseEff :: forall eff effs effs0
            . HasEffect eff effs0
           => EffectParser eff
           -> EffectParsers_ effs effs0
           -> EffectParsers_ (eff ': effs) effs0
  NoEffs :: forall effs0. EffectParsers_ '[] effs0

noParser :: EffectParser eff
noParser = EffectParser Proxy (\_ _ -> rule empty)
