module Data.DynamicValue
  ( Dynamic(..)
  , SomeDynamic(..)
  , UIValue
  , newUIValue
  , modifyUIValue
  , setUIValue
  , getUIValue
  , type DynamicValue
  , SomeUIExpr(..)
  , SomeUIValue(..)
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Value.Parser (ParsedValue(..))

import Control.Concurrent.MVar

-- | A type constructor @f@ is @Dynamic@ when it supports
-- impure reads and writes, and can inform listeners when its
-- value changes.
--
-- NOTE: there is not yet any provision for UN-registering listeners!
class Dynamic (e :: Type -> Type) where
  getDynamic :: e t -> IO t
  setDynamic :: e t -> t -> IO (Maybe String)
  listenWith :: e t -> (t -> t -> IO ()) -> IO ()

instance Dynamic UIValue where
  getDynamic = getUIValue
  setDynamic d v = setUIValue d v >> pure Nothing
  listenWith = onChange

-- | A @SomeDynamic t@ is an instance of @Dynamic@, but we
-- aren't sure /which/ instance. In other words, it's an
-- existential closure over some dynamic value of type @t@.
data SomeDynamic t where
  SomeDynamic :: forall dyn t. Dynamic dyn => dyn t -> SomeDynamic t

instance Dynamic SomeDynamic where
  getDynamic (SomeDynamic d) = getDynamic d
  setDynamic (SomeDynamic d) = setDynamic d
  listenWith (SomeDynamic d) = listenWith d

-- | @UIValue@ is some simple glue that allows us to dynamically
-- read and write configuration values.
newtype UIValue t = UIValue (MVar (t, [t -> t -> IO ()]))

-- | Create a new @UIValue@ with no listeners and the given initial value.
newUIValue :: MonadIO m => t -> m (UIValue t)
newUIValue v = UIValue <$> liftIO (newMVar (v, []))

-- | Register a listener for changes to some @UIValue@.
onChange :: MonadIO m => UIValue t -> (t -> t -> IO ()) -> m ()
onChange (UIValue glue) action =
  liftIO (modifyMVar_ glue (\(v, actions) -> pure (v, action:actions)))

-- | Update a @UIValue@ using the given function, triggering update listeners.
--
-- NOTE: This takes a lock while the listeners are being invoked. If
-- one of the listeners attempts to update this @UIValue@ again, it will
-- deadlock! If you may need to update the @UIValue@ in a handler, do it
-- asynchronously by forking a thread and not waiting for completion.
modifyUIValue :: MonadIO m => UIValue t -> (t -> t) -> m ()
modifyUIValue (UIValue glue) f = liftIO $ modifyMVar_ glue $ \(old, actions) -> do
  let !new = f old
  forM_ actions (\action -> action old new)
  pure (new, actions)

-- | Write a @UIValue@, triggering update listeners.
setUIValue :: MonadIO m => UIValue t -> t -> m ()
setUIValue v = liftIO . modifyUIValue v . const

-- | Read a @UIValue@.
getUIValue :: MonadIO m => UIValue t -> m t
getUIValue (UIValue glue) = fst <$> liftIO (readMVar glue)

-- | @DynamicValue@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data DynamicValue :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue name ty) = SomeDynamic (HaskellType ty)

data SomeUIValue where
  SomeUIValue :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> SomeDynamic (HaskellType ty)
              -> SomeUIValue

data SomeUIExpr where
  SomeUIExpr :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> IO ParsedValue
              -> SomeUIExpr
