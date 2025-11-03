module Data.DynamicValue
  ( Dynamic(..)
  , Variable(..)
  , Mapped(..)
  , AsDynamic(..)
  , newVariable
  , newMapped
  , debugNextVariableId
  , clone
  , setValue
  , setValue'
  , modifyValue
  , watchDynamic
  , getDynamic
  , type DynamicValue
  , Dynamic_
  , Variable_
  , ppDynamic
  ) where

import FractalStream.Prelude
import Language.Type

import qualified Data.Map as Map
import Control.Concurrent

import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable

nextVariableId :: MVar Int
nextVariableId = unsafePerformIO (newMVar 0)

debugNextVariableId :: IO ()
debugNextVariableId = do
  iD <- readMVar nextVariableId
  putStrLn ("next variable id will be " ++ show (iD + 1))

data Variable a = Variable Int (MVar (a, Int, Map Int (a -> IO ())))

instance Show (Variable a) where show (Variable i _) = "@@" ++ show i

instance Eq (Variable a) where
  Variable x _ == Variable y _ = x == y

data Dynamic a where
  Dynamic :: forall a. Variable a -> Dynamic a
  Ap :: forall a b. Dynamic (a -> b) -> Dynamic a -> Dynamic b
  Pure :: forall a. a -> Dynamic a
  Join :: forall a. Dynamic (Dynamic a) -> Dynamic a

ppDynamic :: Dynamic a -> String
ppDynamic = \case
  Dynamic (Variable i _) -> "(dyn #" ++ show i ++ ")"
  Ap f x -> concat ["(ap ", ppDynamic f , " ", ppDynamic x, ")"]
  Pure _ -> "pure"
  Join d -> "(join " ++ ppDynamic d ++ ")"

instance Functor Dynamic where
  fmap f = \case
    Pure x -> Pure (f x)
    x -> Ap (Pure f) x

instance Applicative Dynamic where
  Pure f <*> Pure x = Pure (f x)
  f <*> x = Ap f x
  pure = Pure

instance Monad Dynamic where
  Pure x >>= f = f x
  dx >>= f = Join (f <$> dx)

data Mapped src a = Mapped
  { mapper :: Dynamic (src -> a)
  , source :: Variable src
  , cached :: Dynamic a
  }
  deriving Functor

newMapped :: forall a src io. (Typeable a, MonadIO io)
          => Dynamic (src -> a) -> Variable src -> io (Mapped src a)
newMapped mapper source = do
  cache <- getDynamic (mapper <*> dyn source) >>= newVariable
  let cached = dyn cache
  _ <- watchDynamic mapper $ \f -> do
    src <- getDynamic (dyn source)
    setValue' cache (f src)
  _ <- watchDynamic source $ \src -> do
    f <- getDynamic mapper
    setValue' cache (f src)
  pure Mapped{..}

newVariable :: forall a io. (Typeable a, MonadIO io) => a -> io (Variable a)
newVariable x = liftIO $ do
  iD <- modifyMVar nextVariableId (\i -> let i' = i + 1 in seq i' $ pure (i', i'))
  Variable iD <$> newMVar (x, 0, Map.empty)

clone :: (Typeable a, MonadIO io) => Variable a -> io (Variable a)
clone v = newVariable =<< getDynamic v

class AsDynamic f where
  dyn :: forall a. f a -> Dynamic a

instance AsDynamic Dynamic where dyn = id
instance AsDynamic Variable where dyn = Dynamic
instance AsDynamic (Mapped s) where dyn m = dyn (cached m)

-- | @DynamicValue@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data DynamicValue :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue name ty) = Dynamic (HaskellType ty)

getDynamic :: (AsDynamic f, MonadIO io) => f a -> io a
getDynamic d = case dyn d of
  Dynamic (Variable _ mvar) -> (\(x,_,_) -> x) <$> liftIO (readMVar mvar)
  Ap f x   -> getDynamic f <*> getDynamic x
  Pure x   -> pure x
  Join ddx -> getDynamic ddx >>= getDynamic

setValue :: (MonadIO io, Eq a)
         => Variable a
         -> a
         -> io ()
setValue (Variable _ mvar) x' = liftIO $ do
  mactions <- modifyMVar mvar $ \s@(x, n, actions) ->
    if x == x'
    then pure (s, Nothing)
    else pure ((x', n, actions), Just actions)
  case mactions of
    Nothing -> pure ()
    Just actions -> void (traverse ($ x') actions)

setValue' :: MonadIO io
          => Variable a
          -> a
          -> io ()
setValue' (Variable _ mvar) x' = liftIO $ do
  actions <- modifyMVar mvar (\(_, n, actions) -> pure ((x', n, actions), actions))
  void $ traverse ($ x') actions

modifyValue :: MonadIO io
            => Variable a
            -> (a -> a)
            -> io ()
modifyValue (Variable _ mvar) f = liftIO $ do
  (fx, actions) <- modifyMVar mvar (\(x, n, actions) ->
                                      let fx = f x
                                      in pure ((fx, n, actions), (fx, actions)))
  void $ traverse ($ fx) actions

watchDynamic :: (MonadIO io, AsDynamic f) => f a -> (a -> IO ()) -> io (IO ())
watchDynamic d = liftIO . (`go` dyn d)
  where
    go :: forall t. (t -> IO ()) -> Dynamic t -> IO (IO ())
    go action = \case

      Pure _ -> pure (pure ())

      Ap f x -> do
        update <- newEmptyMVar
        tid <- forkIO $
          let next = takeMVar update >>= \case
                Just (Left  fv) -> do
                  fx <- fv <$> getDynamic x
                  action fx
                  next
                Just (Right xv) -> do
                  fx <- getDynamic f <&> ($ xv)
                  action fx
                  next
                Nothing -> pure ()
          in next
        stopF <- go (void . putMVar update . Just . Left) f
        stopX <- go (void . putMVar update . Just . Right) x
        pure (stopF >> stopX >> putMVar update Nothing >> killThread tid)

      Dynamic (Variable _ mvar) -> do
        n <- modifyMVar mvar $ \(x, n, m) ->
          pure ((x, n + 1, Map.insert n action m), n)
        pure (modifyMVar_ mvar $ \(x', n', m) -> pure (x', n', Map.delete n m))

      Join ddx -> do
        dx0 <- getDynamic ddx
        inner <- newMVar =<< watchDynamic dx0 action
        outerStop <- watchDynamic ddx $ \dx ->
          modifyMVar_ inner $ \oldStop -> do
            -- If the inner dynamic value has changed, run the old
            -- stop action, then set the new stop action.
            oldStop
            getDynamic dx >>= action
            newStop <- watchDynamic dx action
            pure newStop
        pure (join (takeMVar inner) >> outerStop)

-- | @Dynamic_@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data Dynamic_ :: Symbol -> FSType -> Exp Type
type instance Eval (Dynamic_ name ty) = Dynamic (HaskellType ty)

-- | @Variable_@ is a type family that represents
-- a named, variable version of the Haskell type corresponding to
-- the given 'FSType'.
data Variable_ :: Symbol -> FSType -> Exp Type
type instance Eval (Variable_ name ty) = Variable (HaskellType ty)
