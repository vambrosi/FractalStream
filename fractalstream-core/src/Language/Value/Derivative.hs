{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Derivative
  (derivative, derivativeWith, wirtingerWith) where

import FractalStream.Prelude

import Data.Indexed.Functor
import Language.Parser.SourceRange
import Language.Typecheck
import Language.Value

-- | Computes the derivative of a @Value et@ with respect to a variable
-- @z@. When @z@ is real, this is the ordinary real derivative (same type
-- in, same type out -- @derivativeWith@). When @z@ is complex, this is the
-- Wirtinger derivative @∂F/∂z@ (@wirtingerWith@, always complex-valued,
-- regardless of @F@'s own type): it coincides exactly with the ordinary
-- holomorphic derivative whenever @F@ *is* holomorphic (so every existing
-- holomorphic use -- @solve@, closed-form @critical@, @diff@ on a
-- holomorphic expression -- is unaffected), and is additionally defined for
-- @Abs@/@Re@/@Im@/@conj@ and any real-valued @F@, where it captures exactly
-- the "real gradient vanishes" critical-point condition (see the `critical`
-- section of AGENT.md for the derivation). A real-valued function of a
-- complex variable is never holomorphic (Cauchy-Riemann) unless constant,
-- so there is no case where "the same-type derivative" and "the Wirtinger
-- derivative" are two competing correct answers for a complex @z@ -- the
-- Wirtinger one is simply the more general, complete notion.
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

-- | Computes the ordinary (same-type) derivative of a @Value et@ via a
-- caller-supplied rule for free variables ('shadowOf'): for each @Var@ node
-- encountered, it either supplies that variable's derivative directly (e.g.
-- a tracked loop variable's running shadow), or returns 'Nothing' to treat
-- the variable as locally constant (derivative 0). 'derivative' is the
-- special case where exactly one named variable is tracked, with shadow 1.
--
-- This is the right tool when the differentiation variable is real (real
-- calculus has no "held constant" direction to lose, so same-type is
-- already the complete notion), or when it's known in advance that
-- everything involved is holomorphic. For a complex differentiation
-- variable where @Abs@/@Re@/@Im@/@conj@ or a real-valued result might be
-- involved, use 'wirtingerWith' instead.
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

-- | The Wirtinger derivative @∂F/∂z@ of a @Value et@ with respect to a
-- *complex* variable @z@: always complex-valued (see 'derivative''s
-- haddock), regardless of @F@'s own type. 'shadowOf' must likewise always
-- produce a complex shadow for a tracked variable, whatever that
-- variable's own type is.
--
-- The @Abs@/@Re@/@Im@ rules assume their argument is itself holomorphic in
-- @z@ (i.e. @∂arg/∂z̄ = 0@) -- true whenever the argument's own construction
-- never itself passed through @Abs@/@Re@/@Im@/@conj@. That covers a
-- closed-form expression or compound-function body built from ordinary
-- arithmetic with a single non-holomorphic operation applied at the very
-- end (e.g. a Green potential's final @log(|x|)@), which is the case this
-- was built for; nested non-holomorphic operations would need tracking
-- both @∂/∂z@ and @∂/∂z̄@ to get right, which this does not attempt.
wirtingerWith :: forall et. String
              -> (forall et'. Value et' -> Maybe (Value '(Env et', 'ComplexT)))
              -> SourceRange
              -> Value et
              -> TC (Value '(Env et, 'ComplexT))
wirtingerWith blameName shadowOf sr2 v =
  -- `ComplexAt`'s type instance pattern-matches its index as a `'(env,ty)`
  -- tuple, so it doesn't reduce for an abstract `et` on its own; this
  -- rewrites `et` to that tuple shape so `Eval (ComplexAt et)` reduces to
  -- `Value '(Env et, 'ComplexT)` as the type signature above promises.
  case lemmaEnvTy @et of
    Refl -> indexedFoldWithOriginalM @ComplexAt wirtingerRules v
  where
    wirtingerRules :: forall s. ValueF (FIX ValueF :*: ComplexAt) s -> TC (Eval (ComplexAt s))
    wirtingerRules = \case

      Const{} -> pure 0

      Var name ty pf -> case shadowOf (Var name ty pf) of
        Just d  -> pure d
        Nothing -> pure 0

      -- | Basic algebra, real: widen the primal values to complex (the
      -- shadows dx/dy are already complex).
      AddF (_, dx) (_, dy) -> pure $ dx + dy
      SubF (_, dx) (_, dy) -> pure $ dx - dy
      MulF (x, dx) (y, dy) -> pure $ R2C y * dx + R2C x * dy
      DivF (x, dx) (y, dy) -> pure $ (R2C y * dx - R2C x * dy) / (R2C y * R2C y)
      PowF (x, dx) (n, _)  -> pure $ R2C n * R2C x ** (R2C n - 1) * dx
      NegF (_, dx)         -> pure $ negate dx

      -- | Basic algebra, complex: unchanged, already complex throughout.
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

      -- | Non-holomorphic operations: the actual point of Wirtinger
      -- differentiation. Derivations (z̄ held constant, i.e. the argument
      -- assumed holomorphic -- see the haddock above):
      --   |x| = sqrt(x·x̄)         =>  ∂|x|/∂z = x̄·(∂x/∂z) / (2|x|)
      --   Re(x) = (x+x̄)/2         =>  ∂Re(x)/∂z = ½·(∂x/∂z)
      --   Im(x) = (x-x̄)/(2i)      =>  ∂Im(x)/∂z = (∂x/∂z) / (2i)
      --   conj(x) = x̄             =>  ∂(conj x)/∂z = 0  (x̄ is what's held constant)
      AbsC (x, dx) -> pure $ (ConjC x * dx) / (2 * R2C (AbsC x))
      ReC  (_, dx) -> pure $ dx / 2
      ImC  (_, dx) -> pure $ dx / Const (Scalar ComplexType (0 :+ 2))
      ConjC _      -> pure 0

      -- | Real absolute value: d|x|/dx = x/|x| (x /= 0), same "held
      -- constant" reasoning applied to a real argument.
      AbsF (x, dx) -> pure $ (R2C x / R2C (AbsF x)) * dx

      -- | Branches: differentiate each side, keep the (undifferentiated)
      -- condition. Standard AD convention -- ignores the measure-zero
      -- boundary between branches.
      ITE _ (c, _) (_, dyes) (_, dno) -> pure $ ITE ComplexType c dyes dno

      -- | Type conversions: dx is already complex (Integer/Real are never
      -- tracked seeds, and Complex is already this fold's target), so these
      -- are pass-throughs, not further conversions.
      I2R  (_, dx) -> pure dx
      R2C  (_, dx) -> pure dx

      -- C2R2's codomain is a pair, which doesn't fit a fold whose target is
      -- always a single complex value -- falls through to the error below,
      -- same as any other unsupported node.

      _ -> throwError $ DiffNotImplemented sr2 blameName

-- | The fold target for 'wirtingerWith': every index resolves to a single
-- complex value, regardless of the original node's own type.
data ComplexAt :: (Environment, FSType) -> Exp Type
type instance Eval (ComplexAt '(env, ty)) = Value '(env, 'ComplexT)
