module Data.Indexed.FunctorSpec
  ( spec ) where

import Test.Hspec
import Fcf
import Data.Indexed.Functor
import Data.Kind

-----------------------------------------------------
-- An index to determines the type of an AST:
-- either an integer, or a boolean.
-----------------------------------------------------
data T = IntT | BoolT

data TProxy (ty :: T) where
  IsInt  :: TProxy 'IntT
  IsBool :: TProxy 'BoolT

-----------------------------------------------------
-- An indexed functor describing an expression AST.
-- This is built mechanically from the a normal recursive
-- Ast type by adding a type parameter `ast` of kind
-- T -> Exp Type, where T is the index type, and replacing
-- all recursive uses of `Ast i` with `Eval (ast i)`.
-----------------------------------------------------

type Ast = AstF (FIX AstF)

data AstF (ast :: T -> Exp Type) (i :: T) where
  Yes    :: forall ast. AstF ast 'BoolT
  No     :: forall ast. AstF ast 'BoolT
  Number :: forall ast. Int -> AstF ast 'IntT
  Equals :: forall ast t. TProxy t -> Eval (ast t) -> Eval (ast t) -> AstF ast 'BoolT
  Plus   :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT
  Times  :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT

-----------------------------------------------------
-- The IFunctor instance for AstF. We must define
-- a type that acts as a value-level proxy (singleton)
-- for the type index.
-----------------------------------------------------

instance IFunctor AstF where
  type IndexProxy AstF = TProxy

  imap f = \case
    Yes        -> Yes
    No         -> No
    Number n   -> Number n
    Plus x y   -> Plus (f IsInt x) (f IsInt y)
    Times x y  -> Times (f IsInt x) (f IsInt y)
    Equals t x y -> Equals t (f t x) (f t y)

  toIndex = \case
    Yes -> IsBool
    No  -> IsBool
    Number {} -> IsInt
    Plus {}   -> IsInt
    Times {}  -> IsInt
    Equals {} -> IsBool


-----------------------------------------------------
-- An indexed family of types, describing a
-- Haskell type that can be used as the evaluation
-- target for a given index.
-----------------------------------------------------
data HaskellType_ :: T -> Exp Type
type instance Eval (HaskellType_ 'IntT)  = Int
type instance Eval (HaskellType_ 'BoolT) = Bool

type HaskellType t = Eval (HaskellType_ t)

-----------------------------------------------------
-- An evaluator, from Asts to values of the
-- corresponding Haskell type. Computed using
-- an indexed fold, so the recursive values are
-- "already computed" and have the right type.
-----------------------------------------------------
eval :: Ast t -> HaskellType t
eval = indexedFold @HaskellType_ $ \case
  Yes        -> True
  No         -> False  -- Note that the resulting values
  Number n   -> n      -- can have different Haskell types!
  Plus x y   -> x + y
  Times x y  -> x * y
  Equals t x y ->
    case t of           -- Must match on index to get
      IsInt  -> x == y  -- the correct Eq instance for
      IsBool -> x == y  -- each possible type

-----------------------------------------------------
-- An indexed traversable instance for the AST.
-- This will allow us to perform indexed folds
-- that also run monadic effects.
-----------------------------------------------------
instance ITraversable AstF where
  isequence = \case
    Yes -> pure Yes
    No  -> pure No
    Number n -> pure (Number n)
    Plus  mx my -> Plus <$> mx <*> my
    Times mx my -> Times <$> mx <*> my
    Equals t mx my -> Equals t <$> mx <*> my

-----------------------------------------------------
-- An evaluator, from Asts to values of the
-- corresponding Haskell type, that performs
-- the fold in the Maybe monad. This allows for
-- any sub-computation to short-circuit the
-- larger computation.
-- By way of example, we will make Number fail
-- for negative values.
-----------------------------------------------------
evalMaybe :: Ast t -> Maybe (HaskellType t)
evalMaybe = indexedFoldM @HaskellType_ $ \case
  Yes          -> Just True
  No           -> Just False
  Number n
    | n >= 0    -> Just n
    | otherwise -> Nothing
  Plus x y     -> Just (x + y)
  Times x y    -> Just (x * y)
  Equals t x y ->
    case t of
      IsInt  -> Just (x == y)
      IsBool -> Just (x == y)

-----------------------------------------------------

spec :: Spec
spec = do

  describe "Type-safe evaluation of ASTs" $ do

    it "computes the expected Haskell values, of type Bool" $ do
      eval (Equals IsInt (Number 3 `Plus` Number 7) (Number 10)) `shouldBe` True

    it "computes the expected Haskell values, of type Int" $ do
      eval ((Number 3 `Plus` Number 7) `Plus` Number 5) `shouldBe` 15

    it "can perform applicative effects while folding" $ do
      evalMaybe ((Number 3 `Plus` Number 7) `Plus` Number 5) `shouldBe` Just 15
      evalMaybe ((Number 3 `Plus` Number (-7)) `Plus` Number 5) `shouldBe` Nothing
