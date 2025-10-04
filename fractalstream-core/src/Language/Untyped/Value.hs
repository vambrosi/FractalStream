{-# language UndecidableInstances #-}
module Language.Untyped.Value
  ( type Value
  , ValueF(..)
  , Fun(..)
  , ArithOp(..)
  , type ValueWith
  , substitute
  ) where

import Prelude hiding (GT, LT)
--import Control.Monad.State.Strict
import Data.Color
import Data.Recursive
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Fcf (Eval, Exp)

type Value = Eval (FIX ValueF)

type ValueWith a = Ann ValueF a

data Fun = Abs | Exp | Log | Sqrt | Sin | Cos | Tan | Sinh | Cosh | Tanh
         | Arcsin | Arccos | Arctan | Arcsinh | Arccosh | Arctanh
         | Arg | Re | Im | Conj
         | Neg
  deriving (Eq, Ord, Show)

data ArithOp = Add | Sub | Mul | Div | Pow | Mod
             | Arctan2
  deriving (Eq, Ord, Show)

data ValueF (value :: Exp Type)
  = ConstB Bool
  | ConstI Integer
  | ConstF Double
  | ConstC Double Double
  | ConstColor Color
  | Var String
  | PairV (Eval value) (Eval value)
  | ProjV1 (Eval value)
  | ProjV2 (Eval value)
  | Arith ArithOp (Eval value) (Eval value)
  | Ap1 Fun (Eval value)
  | Or (Eval value) (Eval value)
  | And (Eval value) (Eval value)
  | Not (Eval value)
  | ITE (Eval value) (Eval value) (Eval value)
  | RGB (Eval value) (Eval value) (Eval value)
  | Blend (Eval value) (Eval value) (Eval value)
  | InvertRGB (Eval value)
  | Eql (Eval value) (Eval value)
  | NEq (Eval value) (Eval value)
  | LT (Eval value) (Eval value)

deriving instance Eq   (Eval value) => Eq   (ValueF value)
deriving instance Ord  (Eval value) => Ord  (ValueF value)
deriving instance Show (Eval value) => Show (ValueF value)

instance EFunctor ValueF where
  emap f = \case
    ConstB b -> ConstB b
    ConstI n -> ConstI n
    ConstF r -> ConstF r
    ConstC r i -> ConstC r i
    ConstColor c -> ConstColor c
    Var v -> Var v
    PairV x y -> PairV (f x) (f y)
    ProjV1 p -> ProjV1 (f p)
    ProjV2 p -> ProjV2 (f p)
    Arith op x y -> Arith op (f x) (f y)
    Ap1 fun x -> Ap1 fun (f x)
    Or x y -> Or (f x) (f y)
    And x y -> And (f x) (f y)
    Not x -> Not (f x)
    ITE c y n -> ITE (f c) (f y) (f n)
    RGB r g b -> RGB (f r) (f g) (f b)
    Blend t x y -> Blend (f t) (f x) (f y)
    InvertRGB c -> InvertRGB (f c)
    Eql x y -> Eql (f x) (f y)
    NEq x y -> NEq (f x) (f y)
    LT x y -> LT (f x) (f y)

instance ETraversable ValueF where
  etraverse f = \case
    ConstB b -> pure $ ConstB b
    ConstI n -> pure $ ConstI n
    ConstF r -> pure $ ConstF r
    ConstC r i -> pure $ ConstC r i
    ConstColor c -> pure $ ConstColor c
    Var v -> pure $ Var v
    PairV x y -> PairV <$> f x <*> f y
    ProjV1 p -> ProjV1 <$> f p
    ProjV2 p -> ProjV2 <$> f p
    Arith op x y -> Arith op <$> f x <*> f y
    Ap1 fun x -> Ap1 fun <$> f x
    Or x y -> Or <$> f x <*> f y
    And x y -> And <$> f x <*> f y
    Not x -> Not <$> f x
    ITE c y n -> ITE <$> f c <*> f y <*> f n
    RGB r g b -> RGB <$> f r <*> f g <*> f b
    Blend t x y -> Blend <$> f t <*> f x <*> f y
    InvertRGB c -> InvertRGB <$> f c
    Eql x y -> Eql <$> f x <*> f y
    NEq x y -> NEq <$> f x <*> f y
    LT x y -> LT <$> f x <*> f y

--, Functor, Foldable, Traversable)

substitute :: Map String Value -> Value -> Value
substitute m = fold $ \case
  Var s -> Map.findWithDefault (Var s) s m
  other -> other
