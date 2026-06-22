{-# language AllowAmbiguousTypes #-}

-- | Re-index a 'Value' from one environment into another.
--
-- 'reindexValue' takes a value that was typechecked in some source
-- environment @src@ and rebuilds it so that it lives in a target
-- environment @tgt@.  Every variable referenced by the value must be
-- present in @tgt@ with the same type (typically because @tgt@ is an
-- /extension/ of @src@, e.g. @tgt = locals ++ src@).
--
-- This is the keystone of hygienic function inlining: a function body
-- is typechecked against its (clean) definition-site environment, and
-- then re-indexed into the (possibly larger) call-site environment so
-- that the call-site's local variables cannot capture the body's free
-- variables.  The proofs that names are present/absent are re-derived
-- against the target environment, so the result is sound as long as the
-- target really does contain the source's names.
module Language.Value.Reindex
  ( reindexValue
  , reindexValueWith
  , Replacement(..)
  ) where

import FractalStream.Prelude
import Language.Value
import qualified Data.Map as Map

-- | A replacement value for substitution, living in environment @env@.
data Replacement env where
  Replacement :: forall env vty
               . TypeProxy vty -> Value '(env, vty) -> Replacement env

-- | Rebuild a value so that it is indexed by @tgt@ instead of @src@ (pure
-- environment weakening, no substitution). Errors (at runtime) if a referenced
-- variable is not present in the target environment, or if a locally-bound
-- name collides with the target environment; callers are responsible for
-- ensuring neither happens (the inliner uses fresh, bracketed names).
reindexValue
  :: forall src tgt ty
   . EnvironmentProxy tgt
  -> Value '(src, ty)
  -> Value '(tgt, ty)
reindexValue = reindexValueWith (Map.empty :: Map String (Replacement '[]))

-- | Re-index a value, additionally substituting each variable named in the map
-- with the corresponding 'Replacement'. This is how a function call is inlined:
-- every parameter's fresh internal name maps to the argument expression, and
-- the function body (typechecked at its definition site) is re-indexed into the
-- call-site environment with the parameters replaced. Replacements live in the
-- (outermost) target environment and are re-indexed further inward whenever they
-- appear underneath a binder.
reindexValueWith
  :: forall callEnv src tgt ty
   . Map String (Replacement callEnv)
  -> EnvironmentProxy tgt
  -> Value '(src, ty)
  -> Value '(tgt, ty)
reindexValueWith subst = go
  where
    go :: forall s t a. EnvironmentProxy t -> Value '(s, a) -> Value '(t, a)
    go t v0 = withEnvironment t $ case v0 of

      Const x -> Const x

      Var name ty _ -> case Map.lookup (symbolVal name) subst of
        Just (Replacement rty rval) -> case sameHaskellType rty ty of
          Just Refl -> reindexValue t rval
          Nothing   -> error "reindexValueWith: substitution type mismatch"
        Nothing -> case lookupEnv name ty t of
          Found pf' -> Var name ty pf'
          _ -> error "reindexValue: variable not present in target environment"

      LocalLet name vty _ bound bty body ->
        case lookupEnv' name t of
          Absent' pf' -> recallIsAbsent pf' $
            LocalLet name vty pf' (go t bound) bty
                     (go (BindingProxy name vty t) body)
          _ -> error "reindexValue: let-bound name collides with target environment"

      PairV ty x y -> PairV ty (go t x) (go t y)
      ProjV1 ty p  -> ProjV1 ty (go t p)
      ProjV2 ty p  -> ProjV2 ty (go t p)

      List ty xs -> List ty (map (go t) xs)

      AddF x y -> AddF (go t x) (go t y)
      SubF x y -> SubF (go t x) (go t y)
      MulF x y -> MulF (go t x) (go t y)
      DivF x y -> DivF (go t x) (go t y)
      ModF x y -> ModF (go t x) (go t y)
      PowF x y -> PowF (go t x) (go t y)
      AbsF x   -> AbsF (go t x)
      NegF x   -> NegF (go t x)

      ExpF x  -> ExpF (go t x)
      LogF x  -> LogF (go t x)
      SqrtF x -> SqrtF (go t x)

      RoundF x   -> RoundF (go t x)
      FloorF x   -> FloorF (go t x)
      CeilingF x -> CeilingF (go t x)

      SinF x -> SinF (go t x)
      CosF x -> CosF (go t x)
      TanF x -> TanF (go t x)
      ArcsinF x -> ArcsinF (go t x)
      ArccosF x -> ArccosF (go t x)
      ArctanF x -> ArctanF (go t x)
      Arctan2F x y -> Arctan2F (go t x) (go t y)
      SinhF x -> SinhF (go t x)
      CoshF x -> CoshF (go t x)
      TanhF x -> TanhF (go t x)
      ArcsinhF x -> ArcsinhF (go t x)
      ArccoshF x -> ArccoshF (go t x)
      ArctanhF x -> ArctanhF (go t x)

      AddC x y -> AddC (go t x) (go t y)
      SubC x y -> SubC (go t x) (go t y)
      MulC x y -> MulC (go t x) (go t y)
      DivC x y -> DivC (go t x) (go t y)
      PowC x y -> PowC (go t x) (go t y)
      NegC x   -> NegC (go t x)

      ExpC x  -> ExpC (go t x)
      LogC x  -> LogC (go t x)
      SqrtC x -> SqrtC (go t x)

      SinC x -> SinC (go t x)
      CosC x -> CosC (go t x)
      TanC x -> TanC (go t x)
      SinhC x -> SinhC (go t x)
      CoshC x -> CoshC (go t x)
      TanhC x -> TanhC (go t x)

      AbsC x  -> AbsC (go t x)
      ArgC x  -> ArgC (go t x)
      ReC x   -> ReC (go t x)
      ImC x   -> ImC (go t x)
      ConjC x -> ConjC (go t x)

      AddI x y -> AddI (go t x) (go t y)
      SubI x y -> SubI (go t x) (go t y)
      MulI x y -> MulI (go t x) (go t y)
      DivI x y -> DivI (go t x) (go t y)
      ModI x y -> ModI (go t x) (go t y)
      PowI x y -> PowI (go t x) (go t y)
      AbsI x   -> AbsI (go t x)
      NegI x   -> NegI (go t x)

      I2R x  -> I2R (go t x)
      R2C x  -> R2C (go t x)
      C2R2 x -> C2R2 (go t x)
      ToText ty x -> ToText ty (go t x)

      Or x y  -> Or (go t x) (go t y)
      And x y -> And (go t x) (go t y)
      Not x   -> Not (go t x)

      ITE ty b yes no -> ITE ty (go t b) (go t yes) (go t no)

      RGB r g b     -> RGB (go t r) (go t g) (go t b)
      Blend s c1 c2 -> Blend (go t s) (go t c1) (go t c2)
      InvertRGB c   -> InvertRGB (go t c)

      Eql ty x y -> Eql ty (go t x) (go t y)
      NEq ty x y -> NEq ty (go t x) (go t y)

      LTI x y -> LTI (go t x) (go t y)
      LTF x y -> LTF (go t x) (go t y)

      ConcatText xs -> ConcatText (map (go t) xs)

      Join ty xs -> Join ty (map (go t) xs)

      Remove name ty _ lst test ->
        case lookupEnv' name t of
          Absent' pf' -> recallIsAbsent pf' $
            Remove name ty pf' (go t lst) (go (BindingProxy name ty t) test)
          _ -> error "reindexValue: bound name collides with target environment"

      Find name ty _ lst test deflt ->
        case lookupEnv' name t of
          Absent' pf' -> recallIsAbsent pf' $
            Find name ty pf' (go t lst)
                 (go (BindingProxy name ty t) test) (go t deflt)
          _ -> error "reindexValue: bound name collides with target environment"

      Transform name t1 t2 _ lst body ->
        case lookupEnv' name t of
          Absent' pf' -> recallIsAbsent pf' $
            Transform name t1 t2 pf' (go t lst)
                      (go (BindingProxy name t1 t) body)
          _ -> error "reindexValue: bound name collides with target environment"

      Range lo hi -> Range (go t lo) (go t hi)

      Length ty xs -> Length ty (go t xs)

      Index ty cyc xs i -> Index ty cyc (go t xs) (go t i)
