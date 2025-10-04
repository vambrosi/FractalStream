{-# language UndecidableInstances, PolyKinds #-}
module Data.Recursive
  ( FIX
  , EFunctor(..)
  , ETraversable(..)
  , fold
  , foldM
  , unfold
  , unfoldM
  , AnnF(..)
  , type Ann
  , annotation
  ) where

import Control.Monad hiding (foldM)
import Data.Kind
import Fcf (Eval, Exp)


---------------------------------------------------------------------------------
-- Expression functors: functors that take an Exp Type instead of a Type
---------------------------------------------------------------------------------

class EFunctor (f :: Exp Type -> Type) where
  emap :: forall a b. (Eval a -> Eval b) -> f a -> f b

class EFunctor f => ETraversable f where
  etraverse :: forall a b m
             . Applicative m
            => (Eval a -> m (Eval b))
            -> f a
            -> m (f b)

---------------------------------------------------------------------------------
-- Fixpoint of an expression functor
---------------------------------------------------------------------------------

data FIX :: (Exp Type -> Type) -> Exp Type
type instance Eval (FIX f) = f (FIX f)

---------------------------------------------------------------------------------
-- Annotated recursive types
---------------------------------------------------------------------------------

data AnnF (f :: Exp Type -> Type) (a :: Type) (x :: Exp Type) = Ann a (f x)

type Ann f a = Eval (FIX (AnnF f a))

annotation :: Ann f a -> a
annotation (Ann a _) = a

instance EFunctor f => EFunctor (AnnF f a) where
  emap f (Ann a v) = Ann a (emap f v)

instance ETraversable f => ETraversable (AnnF f a) where
  etraverse f (Ann a v) = Ann a <$> etraverse f v

---------------------------------------------------------------------------------
-- Folds and unfolds
---------------------------------------------------------------------------------

fold :: forall a f
      . EFunctor f
     => (f a -> Eval a)
     -> f (FIX f)
     -> Eval a
fold f = let go = f . emap go in go

foldM :: forall a f m. (ETraversable f, Monad m)
      => (f a -> m (Eval a))
      -> f (FIX f)
      -> m (Eval a)
foldM f = let go = (f <=< etraverse go) in go

unfold :: forall a f. EFunctor f
       => (Eval a -> f a)
       -> Eval a
       -> f (FIX f)
unfold f = let go = emap go . f in go

unfoldM :: forall a f m. (ETraversable f, Monad m)
        => (Eval a -> m (f a))
        -> Eval a
        -> m (f (FIX f))
unfoldM f = let go = join . fmap (etraverse go) . f in go
