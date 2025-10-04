{-# language PolyKinds #-}
module Data.Indexed.Functor
  ( IFunctor(..)
  , (:.:)
  , (:+:)
  , (:*:)
  , ITraversable(..)
  , FIX
  , indexedFold
  , indexedFoldM
  , indexedFoldWithOriginal
  , indexedUnfold
  , indexedUnfoldM
  ) where

import Control.Monad
import Fcf
import Data.Kind

class IFunctor (f :: (k -> Exp Type) -> (k -> Type)) where
  type IndexProxy f :: k -> Type
  imap :: forall a b i
        . (forall j. IndexProxy f j -> Eval (a j) -> Eval (b j))
       -> f a i
       -> f b i
  toIndex :: forall a i. f a i -> IndexProxy f i

data (:.:) (m :: Type -> Type) (a :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((m :.: a) i) = m (Eval (a i))

data (:+:) (a :: k -> Exp Type) (b :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((a :+: b) i) = Either (Eval (a i)) (Eval (b i))

data (:*:) (a :: k -> Exp Type) (b :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((a :*: b) i) = (Eval (a i), Eval (b i))

class IFunctor f => ITraversable (f :: (k -> Exp Type) -> (k -> Type)) where

  isequence :: forall i m a
             . Applicative m
            => f (m :.: a) i
            -> m (f a i)
  isequence = itraverse (const id)

  itraverse :: forall i m a b
             . Applicative m
            => (forall j. IndexProxy f j -> Eval (a j) -> m (Eval (b j)))
            -> f a i
            -> m (f b i)
  itraverse f = isequence . imap f

data FIX :: ((k -> Exp Type) -> k -> Type) -> k -> Exp Type
type instance Eval (FIX f i) = f (FIX f) i


indexedFold :: forall a f i. IFunctor f
            => (forall j. f a j -> Eval (a j))
            -> Eval (FIX f i)
            -> Eval (a i)
indexedFold f x =
  let go :: forall k. IndexProxy f k -> Eval (FIX f k) -> Eval (a k)
      go _ = f . imap go
  in go (toIndex x) x

indexedFoldM :: forall a f i m
              . (ITraversable f, Monad m)
             => (forall j. f a j -> m (Eval (a j)))
             -> Eval (FIX f i)
             -> m (Eval (a i))
indexedFoldM f x =
  let go :: forall k. IndexProxy f k -> Eval (FIX f k) -> m (Eval (a k))
      go _ = f <=< itraverse go
  in go (toIndex x) x

indexedUnfold :: forall a f i
               . IFunctor f
              => (forall j. IndexProxy f j -> Eval (a j) -> f a j)
              -> IndexProxy f i
              -> Eval (a i)
              -> Eval (FIX f i)
indexedUnfold f =
  let go :: forall k. IndexProxy f k -> Eval (a k) -> Eval (FIX f k)
      go = \k -> imap go . f k
  in go

indexedUnfoldM :: forall a f i m
               . (IFunctor f, ITraversable f, Monad m)
              => (forall j. IndexProxy f j -> Eval (a j) -> m (f a j))
              -> IndexProxy f i
              -> Eval (a i)
              -> m (Eval (FIX f i))
indexedUnfoldM f =
  let go :: forall k. IndexProxy f k -> Eval (a k) -> m (Eval (FIX f k))
      go = \k x -> itraverse go =<< f k x
  in go

indexedFoldWithOriginal
  :: forall a f i
   . IFunctor f
  => (forall j. f (FIX f :*: a) j -> Eval (a j))
  -> Eval (FIX f i)
  -> Eval (a i)
indexedFoldWithOriginal f =
  snd . indexedFold (\x -> ((imap (const fst) x), f x))
