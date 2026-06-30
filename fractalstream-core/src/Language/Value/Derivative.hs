{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Derivative
  (derivative, derivativeWith) where

import FractalStream.Prelude

import Data.Indexed.Functor
import Language.Parser.SourceRange
import Language.Typecheck
import Language.Value

-- Computes the partial derivative of a Value et with respect to z
derivative :: SourceRange -> Value et -> SourceRange -> Value et -> TC (Value et)
derivative _ (Var zName zty _) sr2 = derivativeWith (symbolVal zName) shadowOf sr2
  where
    -- The only tracked variable is `z` itself, with shadow 1; everything
    -- else (including a same-typed `Var` with a different name) is constant.
    shadowOf :: forall et'. Value et' -> Maybe (Value et')
    shadowOf (Var name ty _)
      | symbolVal name == symbolVal zName = case (ty, zty) of
          (ComplexType, ComplexType) -> Just (Const (Scalar ComplexType 1))
          (RealType, RealType)       -> Just (Const (Scalar RealType 1))
          _                          -> Nothing
    shadowOf _ = Nothing
derivative sr1 _ _ = \_ -> throwError $ DiffInputMustBeVariable sr1

-- | Computes the derivative of a @Value et@ via a caller-supplied rule for
-- free variables ('shadowOf'): for each @Var@ node encountered, it either
-- supplies that variable's derivative directly (e.g. a tracked loop
-- variable's running shadow), or returns 'Nothing' to treat the variable as
-- locally constant (derivative 0). 'derivative' is the special case where
-- exactly one named variable is tracked, with shadow 1.
--
-- @blameName@ is only used to fill in the "with respect to ..." slot of the
-- 'DiffNotImplemented' error when an unsupported node is hit; it does not
-- affect which nodes are supported.
derivativeWith :: String
               -> (forall et'. Value et' -> Maybe (Value et'))
               -> SourceRange
               -> Value et
               -> TC (Value et)
derivativeWith blameName shadowOf sr2 = indexedFoldWithOriginalM derivativeRules
  where
    derivativeRules :: forall s. ValueF (FIX ValueF :*: FIX ValueF) s -> TC (Value s)
    derivativeRules = \case

      -- | Constant case
      Const (Scalar ty _) -> case ty of
        ComplexType -> pure 0
        RealType    -> pure 0
        IntegerType -> pure 0
        _           -> throwError $ DiffNotImplemented sr2 blameName

      -- | A free variable is differentiated via the caller-supplied shadow,
      -- defaulting to constant (0) if it isn't tracked.
      Var name ty pf -> case shadowOf (Var name ty pf) of
        Just d  -> pure d
        Nothing -> case ty of
          ComplexType -> pure $ Const $ Scalar ComplexType 0
          RealType    -> pure $ Const $ Scalar RealType 0
          _           -> throwError $ DiffNotImplemented sr2 blameName

      -- | Basic algebra

      AddF (_, dx) (_, dy) -> pure $ dx + dy
      SubF (_, dx) (_, dy) -> pure $ dx - dy
      MulF (x, dx) (y, dy) -> pure $ y * dx + x * dy
      DivF (x, dx) (y, dy) -> pure $ (y * dx - x * dy) / (y * y)
      PowF (x, dx) (n, _)  -> pure $ n * x ** (n-1) * dx
      NegF (_, dx)         -> pure $ negate dx

      AddC (_, dx) (_, dy) -> pure $ dx + dy
      SubC (_, dx) (_, dy) -> pure $ dx - dy
      MulC (x, dx) (y, dy) -> pure $ y * dx + x * dy
      DivC (x, dx) (y, dy) -> pure $ (y * dx - x * dy) / (y * y)
      PowC (x, dx) (n, _)  -> pure $ n * x ** (n-1) * dx
      NegC (_, dx)         -> pure $ negate dx

      -- | Transcendental functions

      ExpF     (x, dx) -> pure $ dx * ExpF x
      LogF     (x, dx) -> pure $ dx / x
      SqrtF    (x, dx) -> pure $ dx / (2 * SqrtF x)

      CosF     (x, dx) -> pure $ negate dx * SinF x
      SinF     (x, dx) -> pure $ dx * CosF x
      TanF     (x, dx) -> pure $ dx * (1 + TanF x ** 2)

      ArccosF  (x, dx) -> pure $ negate dx * SqrtF (1 - x ** 2)
      ArcsinF  (x, dx) -> pure $ dx * SqrtF (1 - x ** 2)
      ArctanF  (x, dx) -> pure $ dx / (1 + x ** 2)

      CoshF    (x, dx) -> pure $ dx * SinhF x
      SinhF    (x, dx) -> pure $ dx * CoshF x
      TanhF    (x, dx) -> pure $ dx * (1 - TanhF x ** 2)

      ArccoshF (x, dx) -> pure $ negate dx * SqrtF (x ** 2 - 1)
      ArcsinhF (x, dx) -> pure $ dx * SqrtF (1 + x ** 2)
      ArctanhF (x, dx) -> pure $ dx / (1 - x ** 2)

      ExpC     (x, dx) -> pure $ dx * ExpC x
      LogC     (x, dx) -> pure $ dx / x
      SqrtC    (x, dx) -> pure $ dx / (2 * SqrtC x)

      CosC     (x, dx) -> pure $ negate dx * SinC x
      SinC     (x, dx) -> pure $ dx * CosC x
      TanC     (x, dx) -> pure $ dx * (1 + TanC x ** 2)

      CoshC    (x, dx) -> pure $ dx * SinhC x
      SinhC    (x, dx) -> pure $ dx * CoshC x
      TanhC    (x, dx) -> pure $ dx * (1 - TanhC x ** 2)

      -- | Type conversions

      I2R  (_, dx) -> pure $ I2R dx
      R2C  (_, dx) -> pure $ R2C dx
      C2R2 (_, dx) -> pure $ C2R2 dx

      _ -> throwError $ DiffNotImplemented sr2 blameName