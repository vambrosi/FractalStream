{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Derivative
  (derivative, derivativeWith, wirtingerWith) where

import FractalStream.Prelude

import Data.Indexed.Functor
import Language.Parser.SourceRange
import Language.Typecheck
import Language.Value

-- | The derivative of a value with respect to a variable @z@.
--
-- * For real @z@, the ordinary derivative (same type in and out).
-- * For complex @z@, the Wirtinger derivative @∂F/∂z@ (always complex).
--   It equals the usual derivative for holomorphic @F@, and is also defined
--   for @|.|@, @re@, @im@, @conj@ and real-valued @F@. For real @F@,
--   @∂F/∂z = 0@ exactly when the real gradient vanishes.
derivative :: SourceRange -> Value et -> SourceRange -> Value et -> TC (Value et)
derivative _ (Var zName zty _) sr2 = case zty of
  RealType -> derivativeWith (symbolVal zName) shadowOfSame sr2
    where
      shadowOfSame :: forall et'. Value et' -> Maybe (Value et')
      shadowOfSame (Var name ty _)
        | symbolVal name == symbolVal zName = case ty of
            RealType -> Just (Const (Scalar RealType 1))
            _        -> Nothing
      shadowOfSame _ = Nothing
  ComplexType -> wirtingerWith (symbolVal zName) shadowOfWirtinger sr2
    where
      shadowOfWirtinger :: forall et'. Value et' -> Maybe (Value '(Env et', 'ComplexT))
      shadowOfWirtinger (Var name ty _)
        | symbolVal name == symbolVal zName = case ty of
            ComplexType -> Just (Const (Scalar ComplexType 1))
            _           -> Nothing
      shadowOfWirtinger _ = Nothing
  _ -> \_ -> throwError $ DiffNotImplemented sr2 (symbolVal zName)
derivative sr1 _ _ = \_ -> throwError $ DiffInputMustBeVariable sr1

-- | The ordinary (same-type) derivative. @shadowOf@ gives each variable's
-- derivative, or 'Nothing' for a constant. Use 'wirtingerWith' for a complex
-- variable when @F@ may not be holomorphic.
--
-- @blameName@ only fills in the error message for unsupported nodes.
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

-- | The Wirtinger derivative @∂F/∂z@ for complex @z@. The result and all
-- shadows are complex, whatever their own types.
--
-- The @|.|@/@re@/@im@ rules assume their argument is holomorphic
-- (@∂arg/∂z̄ = 0@), so a non-holomorphic operation is only correct at the
-- end of a computation (e.g. @log(|x|)@), not nested inside another one.
wirtingerWith :: forall et. String
              -> (forall et'. Value et' -> Maybe (Value '(Env et', 'ComplexT)))
              -> SourceRange
              -> Value et
              -> TC (Value '(Env et, 'ComplexT))
wirtingerWith blameName shadowOf sr2 v =
  -- Exposes `et` as a tuple, so `Eval (ComplexAt et)` reduces.
  case lemmaEnvTy @et of
    Refl -> indexedFoldWithOriginalM @ComplexAt wirtingerRules v
  where
    wirtingerRules :: forall s. ValueF (FIX ValueF :*: ComplexAt) s -> TC (Eval (ComplexAt s))
    wirtingerRules = \case

      Const{} -> pure 0

      Var name ty pf -> case shadowOf (Var name ty pf) of
        Just d  -> pure d
        Nothing -> pure 0

      -- | Basic algebra, real (widen the values, since shadows are complex).
      AddF (_, dx) (_, dy) -> pure $ dx + dy
      SubF (_, dx) (_, dy) -> pure $ dx - dy
      MulF (x, dx) (y, dy) -> pure $ R2C y * dx + R2C x * dy
      DivF (x, dx) (y, dy) -> pure $ (R2C y * dx - R2C x * dy) / (R2C y * R2C y)
      PowF (x, dx) (n, _)  -> pure $ R2C n * R2C x ** (R2C n - 1) * dx
      NegF (_, dx)         -> pure $ negate dx

      -- | Basic algebra, complex
      AddC (_, dx) (_, dy) -> pure $ dx + dy
      SubC (_, dx) (_, dy) -> pure $ dx - dy
      MulC (x, dx) (y, dy) -> pure $ y * dx + x * dy
      DivC (x, dx) (y, dy) -> pure $ (y * dx - x * dy) / (y * y)
      PowC (x, dx) (n, _)  -> pure $ n * x ** (n-1) * dx
      NegC (_, dx)         -> pure $ negate dx

      -- | Transcendental functions, real (widened)

      ExpF     (x, dx) -> pure $ dx * ExpC (R2C x)
      LogF     (x, dx) -> pure $ dx / R2C x
      SqrtF    (x, dx) -> pure $ dx / (2 * SqrtC (R2C x))

      CosF     (x, dx) -> pure $ negate dx * SinC (R2C x)
      SinF     (x, dx) -> pure $ dx * CosC (R2C x)
      TanF     (x, dx) -> pure $ dx * (1 + TanC (R2C x) ** 2)

      ArccosF  (x, dx) -> pure $ negate dx * SqrtC (1 - R2C x ** 2)
      ArcsinF  (x, dx) -> pure $ dx * SqrtC (1 - R2C x ** 2)
      ArctanF  (x, dx) -> pure $ dx / (1 + R2C x ** 2)

      CoshF    (x, dx) -> pure $ dx * SinhC (R2C x)
      SinhF    (x, dx) -> pure $ dx * CoshC (R2C x)
      TanhF    (x, dx) -> pure $ dx * (1 - TanhC (R2C x) ** 2)

      ArccoshF (x, dx) -> pure $ negate dx * SqrtC (R2C x ** 2 - 1)
      ArcsinhF (x, dx) -> pure $ dx * SqrtC (1 + R2C x ** 2)
      ArctanhF (x, dx) -> pure $ dx / (1 - R2C x ** 2)

      -- | Transcendental functions, complex (unchanged)

      ExpC     (x, dx) -> pure $ dx * ExpC x
      LogC     (x, dx) -> pure $ dx / x
      SqrtC    (x, dx) -> pure $ dx / (2 * SqrtC x)

      CosC     (x, dx) -> pure $ negate dx * SinC x
      SinC     (x, dx) -> pure $ dx * CosC x
      TanC     (x, dx) -> pure $ dx * (1 + TanC x ** 2)

      CoshC    (x, dx) -> pure $ dx * SinhC x
      SinhC    (x, dx) -> pure $ dx * CoshC x
      TanhC    (x, dx) -> pure $ dx * (1 - TanhC x ** 2)

      -- | Non-holomorphic operations, for holomorphic @x@:
      --   |x| = sqrt(x·x̄)         =>  ∂|x|/∂z = x̄·(∂x/∂z) / (2|x|)
      --   Re(x) = (x+x̄)/2         =>  ∂Re(x)/∂z = ½·(∂x/∂z)
      --   Im(x) = (x-x̄)/(2i)      =>  ∂Im(x)/∂z = (∂x/∂z) / (2i)
      --   conj(x) = x̄             =>  ∂(conj x)/∂z = 0  (x̄ is what's held constant)
      AbsC (x, dx) -> pure $ (ConjC x * dx) / (2 * R2C (AbsC x))
      ReC  (_, dx) -> pure $ dx / 2
      ImC  (_, dx) -> pure $ dx / Const (Scalar ComplexType (0 :+ 2))
      ConjC _      -> pure 0

      -- | Real absolute value (d|x|/dx = x/|x|).
      AbsF (x, dx) -> pure $ (R2C x / R2C (AbsF x)) * dx

      -- | Branches. (Differentiate each side and keep the condition; the
      -- boundary between branches is ignored.)
      ITE _ (c, _) (_, dyes) (_, dno) -> pure $ ITE ComplexType c dyes dno

      -- | Booleans (never tracked, so 0). Needed because the fold visits
      -- every child, including the unused condition of an `if`.
      Or{}  -> pure 0
      And{} -> pure 0
      Not{} -> pure 0
      Eql{} -> pure 0
      NEq{} -> pure 0
      LTI{} -> pure 0
      LTF{} -> pure 0

      -- | Integers (never tracked, so 0). They can occur inside a
      -- differentiated expression, e.g. `x / 2^n` for a loop counter `n`.
      RoundF{}   -> pure 0
      FloorF{}   -> pure 0
      CeilingF{} -> pure 0
      AddI{}     -> pure 0
      SubI{}     -> pure 0
      MulI{}     -> pure 0
      DivI{}     -> pure 0
      ModI{}     -> pure 0
      PowI{}     -> pure 0
      AbsI{}     -> pure 0
      NegI{}     -> pure 0
      Length{}   -> pure 0

      -- | Conversions (dx is already complex, so pass it through).
      I2R  (_, dx) -> pure dx
      R2C  (_, dx) -> pure dx

      -- C2R2 (a pair) is unsupported and falls through to the error.

      _ -> throwError $ DiffNotImplemented sr2 blameName

-- | The fold target for 'wirtingerWith' (a complex value at every type).
data ComplexAt :: (Environment, FSType) -> Exp Type
type instance Eval (ComplexAt '(env, ty)) = Value '(env, 'ComplexT)
