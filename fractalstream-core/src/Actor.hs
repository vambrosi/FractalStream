{-# language UndecidableInstances #-}
module Actor
  ( type ToFun(..)
  ) where

import FractalStream.Prelude

import Language.Code
import Language.Value.Evaluator (HaskellTypeOfBinding)


class ToFun (env :: Environment) (result :: Type) where
  type ToFunction env result :: Type
  toFunction :: (Context HaskellTypeOfBinding env -> result)
             -> ToFunction env result

instance ToFun '[] result where
  type ToFunction '[] result = result
  toFunction k = k EmptyContext

instance (KnownSymbol name, KnownType ty, NotPresent name env, ToFun env result)
    => ToFun ( '(name, ty) ': env ) result where
  type ToFunction ( '(name, ty) ': env ) result
    = HaskellType ty -> ToFunction env result
  toFunction k = \x ->
    toFunction @env @result (k . Bind (Proxy @name) (typeProxy @ty) x)
