{-# language AllowAmbiguousTypes #-}

-- | Building blocks for differentiating a value through a /tracked/
-- variable's shadow, rather than a single fixed target.
--
-- A tracked variable's derivative lives in another, genuinely separate
-- runtime variable (not a symbolic expression -- the dual-number transform
-- over looped 'Code' needs this, since the loop body that updates the
-- original variable runs an unknown number of times). 'dValue'
-- differentiates a single (non-looped) 'Value' under this convention;
-- 'dualizeCode' extends it to looped 'Code'.
--
-- Shadow names are always freshly generated (via
-- 'Language.Code.Typecheck.withFresh', the same "guaranteed collision-free"
-- mechanism used throughout the typechecker), never derived from the
-- original name by a fixed string convention. 'Tracked' records the
-- resulting name -> shadow-name association explicitly. This is what makes
-- the transform safe to apply more than once to the same code (e.g. to get
-- a second derivative by differentiating an already-dualized program) --
-- there is no fixed prefix a second pass could collide with, at any depth.
module Language.Code.Dual
  ( Tracked
  , dValue
  , dualizeCode
  , spliceCompoundDual
  , dValueWirtinger
  , dualizeCodeWirtinger
  , spliceCompoundDualWirtinger
  , tcSolveCompound
  , tcCriticalCompound
  ) where

import FractalStream.Prelude
import Language.Value
import Language.Value.Typecheck
  ( ParsedValue(..), atType, tcVar, internalIterationLimit
  , InternalIterations, InternalStuck, InternalSolution )
import Language.Value.Derivative (derivativeWith, wirtingerWith)
import Language.Value.Reindex (reindexValue)
import Language.Code
import Language.Code.Reindex (reindexCode)
import Language.Code.Typecheck
  ( CompoundFunction(..), letBind, atEnv, checkPure, defaultFor, inferArgType
  , withFresh, solveTolerance, CheckedCode )
import Language.Typecheck
import Language.Parser.SourceRange
import qualified Data.Map as Map

-- | Tracked variables currently being differentiated, mapping each
-- variable's name to the name of the (already-declared, in-scope) runtime
-- variable holding its derivative.
type Tracked = Map String String

-- | A shadow name for @name@, salted by @gen@ -- a caller-supplied
-- identifier unique to /one differentiation pass/ (in practice, the
-- pass's own freshly-generated seed-shadow name, itself already guaranteed
-- unique by 'Language.Code.Typecheck.withFresh'). Names within one pass
-- are distinguished by @name@ itself, which is already guaranteed unique
-- within any single valid 'Code' (the typechecker rejects re-declaring a
-- name).
--
-- This -- not 'Language.Code.Typecheck.withFresh' -- is how 'dualizeCode'
-- names new shadows. @withFresh@'s freshness is only relative to the
-- environment it's given, i.e. to names already threaded into that
-- specific chain; it can't see names sitting deeper, un-reindexed, inside
-- an already-built nested @Let@-chain (e.g. the output of an earlier
-- 'dualizeCode' pass). Two independent passes over overlapping code,
-- started from environments of similar apparent size, can and did produce
-- the exact same @withFresh@ name for two different purposes. Salting by
-- @gen@ makes that structurally impossible: two passes with different
-- @gen@s can never choose the same name, regardless of environment shape.
freshShadowName :: String -> String -> String
freshShadowName gen name = "[dual " ++ gen ++ " of " ++ name ++ "]"

-- | Differentiate @v@ with respect to the seed direction, using @tracked@:
-- a tracked variable's derivative is read from its shadow; anything else
-- (an untracked variable, a constant, ...) is locally constant (0).
-- @blame@ only fills the "with respect to ..." slot of a
-- 'DiffNotImplemented' error (e.g. when @v@ contains a loop or an
-- unsupported node).
dValue :: String -> Tracked -> SourceRange -> Value et -> TC (Value et)
dValue blame tracked sr v = derivativeWith blame shadowOf sr v
  where
    shadowOf :: forall et'. Value et' -> Maybe (Value et')
    shadowOf (Var name ty _) = case Map.lookup (symbolVal name) tracked of
      Nothing -> Nothing
      Just shadowNm -> case someSymbolVal shadowNm of
        SomeSymbol sname -> case lookupEnv sname ty (envProxy Proxy) of
          Found pf' -> Just (Var sname ty pf')
          _         -> Nothing
    shadowOf _ = Nothing

-- | Real or Complex: the two types 'dValue'/'dualizeCode' can differentiate
-- and track a shadow for. Everything else (Integer loop counters, Boolean
-- flags, ...) has no meaningful derivative and is left completely alone.
isDifferentiable :: TypeProxy ty -> Bool
isDifferentiable = \case
  ComplexType -> True
  RealType    -> True
  _           -> False

-- | Transform a 'Code' so that every Real/Complex @Let@/@Set@-bound
-- variable named in @tracked@ (extended with a fresh shadow as new @Let@s
-- are discovered) gets a shadow holding its running derivative with
-- respect to the seed direction. A tracked variable must already have its
-- shadow declared (with its initial derivative) in the environment @code@
-- starts in and recorded in @tracked@; @dualizeCode@ only introduces
-- shadows for variables @code@ itself declares via @Let@.
--
-- @blame@ only fills the "with respect to ..." slot of a
-- 'DiffNotImplemented' error. @gen@ salts every new shadow name (see
-- 'freshShadowName') -- pass something unique to this differentiation pass
-- (e.g. the seed's own shadow name). Only @Let@/@Set@/@Block@/
-- @IfThenElse@/@DoWhile@/@NoOp@ are supported; @ForEach@/@Lookup@/
-- @DrawCommand@ are rejected with a clear error (not used by
-- potential-style numeric loop bodies).
dualizeCode :: SourceRange -> String -> String -> Tracked -> Code env -> TC (Code env)
dualizeCode sr blame gen tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        dv <- dValue blame tracked sr v
        letBind sr (symbolVal name) ty v env $ \env1 ->
          letBind sr (freshShadowName gen (symbolVal name)) ty (reindexValue env1 dv) env1 $ \env2 ->
            dualizeCode sr blame gen
              (Map.insert (symbolVal name) (freshShadowName gen (symbolVal name)) tracked)
              (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCode sr blame gen tracked body

  Set pf name v -> case Map.lookup (symbolVal name) tracked of
    Just shadowNm | isDifferentiable (typeOfValue v) -> do
      let ty = typeOfValue v
      dv <- dValue blame tracked sr v
      case someSymbolVal shadowNm of
        SomeSymbol sname -> case lookupEnv sname ty (envProxy Proxy) of
          -- `dv` (and `v`, if `v` reads `name` itself, e.g. `w <- w * x`)
          -- is evaluated against `name`'s OLD value, so the shadow must
          -- be updated before `name` itself is overwritten below.
          Found spf -> pure (Block [ Set spf sname dv, Set pf name v ])
          _ -> throwError (Advice sr
                 ("dualizeCode: internal error, `" ++ symbolVal name
                   ++ "` is tracked but its shadow is missing."))
    _ -> pure (Set pf name v)

  Block stmts -> Block <$> traverse (dualizeCode sr blame gen tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCode sr blame gen tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCode sr blame gen tracked yes <*> dualizeCode sr blame gen tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCode: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCode: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCode: list operations are not supported.")

-- | Wirtinger analogue of 'dValue': the derivative is always complex,
-- regardless of @v@'s own type (see
-- 'Language.Value.Derivative.wirtingerWith' -- this is exactly that
-- generalization, applied through a tracked shadow instead of a single
-- fixed target). Needed because a compound function's body can be
-- real-valued partway through (a Green potential's final @log(|x|)@, say)
-- while still being differentiated with respect to a complex seed. A
-- tracked variable's shadow is looked up at 'ComplexType', not its own
-- type -- under this convention every shadow is complex, never "whatever
-- type the original variable happened to be".
dValueWirtinger :: String -> Tracked -> SourceRange -> Value et -> TC (Value '(Env et, 'ComplexT))
dValueWirtinger blame tracked sr v = wirtingerWith blame shadowOf sr v
  where
    shadowOf :: forall et'. Value et' -> Maybe (Value '(Env et', 'ComplexT))
    shadowOf (Var name _ _) = case Map.lookup (symbolVal name) tracked of
      Nothing -> Nothing
      Just shadowNm -> case someSymbolVal shadowNm of
        SomeSymbol sname -> case lookupEnv sname ComplexType (envProxy Proxy) of
          Found pf' -> Just (Var sname ComplexType pf')
          _         -> Nothing
    shadowOf _ = Nothing

-- | Wirtinger analogue of 'dualizeCode': structurally identical, but every
-- shadow 'dualizeCodeWirtinger' introduces is declared at 'ComplexType'
-- (via 'dValueWirtinger'), regardless of the tracked variable's own type --
-- see 'dValueWirtinger'.
dualizeCodeWirtinger :: SourceRange -> String -> String -> Tracked -> Code env -> TC (Code env)
dualizeCodeWirtinger sr blame gen tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        dv <- dValueWirtinger blame tracked sr v
        letBind sr (symbolVal name) ty v env $ \env1 ->
          letBind sr (freshShadowName gen (symbolVal name)) ComplexType (reindexValue env1 dv) env1 $ \env2 ->
            dualizeCodeWirtinger sr blame gen
              (Map.insert (symbolVal name) (freshShadowName gen (symbolVal name)) tracked)
              (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCodeWirtinger sr blame gen tracked body

  Set pf name v -> case Map.lookup (symbolVal name) tracked of
    Just shadowNm | isDifferentiable (typeOfValue v) -> do
      dv <- dValueWirtinger blame tracked sr v
      case someSymbolVal shadowNm of
        SomeSymbol sname -> case lookupEnv sname ComplexType (envProxy Proxy) of
          Found spf -> pure (Block [ Set spf sname dv, Set pf name v ])
          _ -> throwError (Advice sr
                 ("dualizeCodeWirtinger: internal error, `" ++ symbolVal name
                   ++ "` is tracked but its shadow is missing."))
    _ -> pure (Set pf name v)

  Block stmts -> Block <$> traverse (dualizeCodeWirtinger sr blame gen tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCodeWirtinger sr blame gen tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCodeWirtinger sr blame gen tracked yes <*> dualizeCodeWirtinger sr blame gen tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCodeWirtinger: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCodeWirtinger: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCodeWirtinger: list operations are not supported.")

-- | Bind each argument to a compound function call to a fresh name (as
-- 'Language.Code.Typecheck.spliceArgs' does for an ordinary, non-dual
-- call), and additionally bind its derivative (with respect to @tracked0@,
-- fixed across all arguments -- an argument expression can't reference
-- another argument's fresh name, since all arguments are evaluated in the
-- same outer scope) to a fresh (salted by @gen@ -- see 'freshShadowName')
-- shadow right alongside it. The continuation's 'Tracked' is @tracked0@
-- plus every differentiable argument's fresh name -> shadow-name
-- association.
spliceArgsDual
  :: forall env
   . SourceRange
  -> String
  -> String
  -> Tracked
  -> [(String, Maybe SomeType, ParsedValue)]
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> TC (Code env'))
  -> TC (Code env)
spliceArgsDual _  _     _   tracked0 [] env k = withEnvironment env (k env tracked0)
spliceArgsDual sr blame gen tracked0 ((fresh, ann, arg) : rest) env k = withEnvironment env $ do
  SomeType (pty :: TypeProxy pty) <- inferArgType sr ann arg env
  withKnownType pty $ do
    argVal <- atType arg pty :: TC (Value '(env, pty))
    dArg   <- dValue blame tracked0 sr argVal
    letBind sr fresh pty argVal env $ \env1 -> case pty of
      ComplexType ->
        letBind sr (freshShadowName gen fresh) pty (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDual sr blame gen tracked0 rest env2
            (\envF trackedF -> k envF (Map.insert fresh (freshShadowName gen fresh) trackedF))
      RealType ->
        letBind sr (freshShadowName gen fresh) pty (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDual sr blame gen tracked0 rest env2
            (\envF trackedF -> k envF (Map.insert fresh (freshShadowName gen fresh) trackedF))
      _ -> spliceArgsDual sr blame gen tracked0 rest env1 k

-- | Splice a compound function call, differentiated with respect to
-- whichever variables are already tracked in @env@ (their shadows must
-- already be declared there and recorded in @tracked0@ -- e.g. the Newton
-- unknown, seeded to 1), producing @(F, F')@ as materialized
-- (@Set@-mutated) local variables of type @ty@. @F@/@F'@ are handed to a
-- continuation rather than copied into a target variable, unlike
-- 'Language.Code.Typecheck.tcSetCompound' (which this otherwise mirrors)
-- -- a Newton loop needs them as ordinary expressions, and they have to be
-- materialized (not symbolic) because the function body may contain a
-- loop. @gen@ salts every fresh shadow name this introduces (see
-- 'freshShadowName') -- pass something unique to this differentiation
-- pass, e.g. @tracked0@'s own seed shadow name.
spliceCompoundDual
  :: SourceRange
  -> String
  -> String
  -> Tracked
  -> CompoundFunction
  -> [ParsedValue]
  -> TypeProxy ty
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Value '(env', ty) -> Value '(env', ty) -> TC (Code env'))
  -> TC (Code env)
spliceCompoundDual sr blame gen tracked0 cf args ty env k
  | length args /= length (cfParams cf) =
      throwError (Advice sr ("The function " ++ cfName cf ++ " expects "
        ++ show (length (cfParams cf)) ++ " argument(s), but "
        ++ show (length args) ++ " were given."))
  | otherwise = withEnvironment env $
      spliceArgsDual sr blame gen tracked0
        (zip3 (cfFreshParams cf) (map snd (cfParams cf)) args) env $ \envP tracked ->
        withKnownType ty $ do
          dflt <- defaultFor sr ty
          letBind sr (cfResultName cf) ty dflt envP $ \envR -> withEnvironment envR $ do
            dfltShadow <- defaultFor sr ty
            let dres = freshShadowName gen (cfResultName cf)
            letBind sr dres ty dfltShadow envR $ \envRD -> withEnvironment envRD $ do
                body  <- atEnv envRD (cfBody cf)
                checkPure cf sr body
                dbody <- dualizeCode sr blame gen
                           (Map.insert (cfResultName cf) dres tracked) body
                case (someSymbolVal (cfResultName cf), someSymbolVal dres) of
                  (SomeSymbol res, SomeSymbol dresProxy) -> do
                    resPf  <- findVarAtType sr res       ty envRD
                    dresPf <- findVarAtType sr dresProxy ty envRD
                    restCode <- k envRD (Var res ty resPf) (Var dresProxy ty dresPf)
                    pure (Block [ dbody, restCode ])

-- | Wirtinger analogue of 'spliceArgsDual': every argument's shadow is
-- declared at 'ComplexType' (via 'dValueWirtinger'), regardless of the
-- argument's own type -- see 'dValueWirtinger'.
spliceArgsDualWirtinger
  :: forall env
   . SourceRange
  -> String
  -> String
  -> Tracked
  -> [(String, Maybe SomeType, ParsedValue)]
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> TC (Code env'))
  -> TC (Code env)
spliceArgsDualWirtinger _  _     _   tracked0 [] env k = withEnvironment env (k env tracked0)
spliceArgsDualWirtinger sr blame gen tracked0 ((fresh, ann, arg) : rest) env k = withEnvironment env $ do
  SomeType (pty :: TypeProxy pty) <- inferArgType sr ann arg env
  withKnownType pty $ do
    argVal <- atType arg pty :: TC (Value '(env, pty))
    dArg   <- dValueWirtinger blame tracked0 sr argVal
    letBind sr fresh pty argVal env $ \env1 -> case pty of
      ComplexType ->
        letBind sr (freshShadowName gen fresh) ComplexType (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDualWirtinger sr blame gen tracked0 rest env2
            (\envF trackedF -> k envF (Map.insert fresh (freshShadowName gen fresh) trackedF))
      RealType ->
        letBind sr (freshShadowName gen fresh) ComplexType (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDualWirtinger sr blame gen tracked0 rest env2
            (\envF trackedF -> k envF (Map.insert fresh (freshShadowName gen fresh) trackedF))
      _ -> spliceArgsDualWirtinger sr blame gen tracked0 rest env1 k

-- | Wirtinger analogue of 'spliceCompoundDual': @F@ (the compound
-- function's own result) stays at its own natural type @ty@ (whatever the
-- caller asks for, same as before -- typically still forced to the
-- unknown's type, e.g. by R2C-widening a real result, since that widening
-- is harmless: 'wirtingerWith''s @R2C@ rule is a pass-through, and its
-- fold already reaches every nested node's own type regardless of what the
-- top-level type tag says). Only @F'@ (the shadow) is forced to
-- 'ComplexType' unconditionally -- see 'dValueWirtinger'.
spliceCompoundDualWirtinger
  :: SourceRange
  -> String
  -> String
  -> Tracked
  -> CompoundFunction
  -> [ParsedValue]
  -> TypeProxy ty
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Value '(env', ty) -> Value '(env', 'ComplexT) -> TC (Code env'))
  -> TC (Code env)
spliceCompoundDualWirtinger sr blame gen tracked0 cf args ty env k
  | length args /= length (cfParams cf) =
      throwError (Advice sr ("The function " ++ cfName cf ++ " expects "
        ++ show (length (cfParams cf)) ++ " argument(s), but "
        ++ show (length args) ++ " were given."))
  | otherwise = withEnvironment env $
      spliceArgsDualWirtinger sr blame gen tracked0
        (zip3 (cfFreshParams cf) (map snd (cfParams cf)) args) env $ \envP tracked ->
        withKnownType ty $ do
          dflt <- defaultFor sr ty
          letBind sr (cfResultName cf) ty dflt envP $ \envR -> withEnvironment envR $ do
            dfltShadow <- defaultFor sr ComplexType
            let dres = freshShadowName gen (cfResultName cf)
            letBind sr dres ComplexType dfltShadow envR $ \envRD -> withEnvironment envRD $ do
                body  <- atEnv envRD (cfBody cf)
                checkPure cf sr body
                dbody <- dualizeCodeWirtinger sr blame gen
                           (Map.insert (cfResultName cf) dres tracked) body
                case (someSymbolVal (cfResultName cf), someSymbolVal dres) of
                  (SomeSymbol res, SomeSymbol dresProxy) -> do
                    resPf  <- findVarAtType sr res       ty          envRD
                    dresPf <- findVarAtType sr dresProxy ComplexType envRD
                    restCode <- k envRD (Var res ty resPf) (Var dresProxy ComplexType dresPf)
                    pure (Block [ dbody, restCode ])

-- | @solve z -> f(args)@ where @f@ is a compound (looped) function: the
-- closed-form 'Language.Code.Typecheck.tcSolve' can't differentiate a loop
-- (@diffClosedForm@ rejects it), so this differentiates @f@'s body via
-- 'dualizeCode' instead.
--
-- Newton needs @F(z)@ and @F'(z)@ fresh at the *current* @z@ on every
-- convergence check, but a looped @F@ can't be represented as a reusable
-- symbolic expression the way 'Language.Code.Typecheck.tcSolve''s
-- closed-form case can -- computing it means actually running the spliced
-- body. So @F@/@F'@ are materialized (@Set@-mutated) local variables, and
-- the splice runs twice per Newton step: once to seed the very first
-- convergence check, and once more each loop iteration (right after @z@
-- moves) so the next check sees the new value. This costs one extra full
-- evaluation of @F@ per step compared to a closed-form @solve@ -- the same
-- cost a finite-difference approach would have paid.
--
-- @solve … continuing seed@ is not yet supported here.
tcSolveCompound :: String
                -> (CompoundFunction, [ParsedValue])
                -> Maybe ParsedValue
                -> Maybe ParsedValue
                -> Bool
                -> CheckedCode
tcSolveCompound var (cf, args) mtol mlimit continuing sr (env :: EnvironmentProxy env)
  | continuing = throwError (Advice sr
      "`solve … continuing seed` is not yet supported for a looped (compound-function) equation.")
  | otherwise = do

    SomeSymbol zname <- pure (someSymbolVal var)
    FoundVar (zty :: TypeProxy zty) zpfEnv <- findVar sr zname env

    withFresh sr env zty (Var zname zty zpfEnv) $ \envS (saveName :: Proxy saveName) pfS -> recallIsAbsent pfS $
     withFresh sr envS IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

      limitValue <- case mlimit of
        Just (ParsedValue _ limitFun) -> limitFun IntegerType
        Nothing -> tcVar internalIterationLimit sr IntegerType

      withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

        let limit ::
              Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': '(saveName, zty) ': env, 'IntegerT)
            limit = Var limitName IntegerType (bindName limitName IntegerType pf')

        (tol :: Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': '(saveName, zty) ': env, 'RealT)) <-
          case mtol of
            Just pv -> atType pv RealType
            Nothing -> pure (Const (Scalar RealType solveTolerance))

        case zty of

          ComplexType -> do
            fDflt <- defaultFor sr ComplexType
            withFresh sr env'' ComplexType fDflt $ \envF (fName :: Proxy fName) fAbs -> recallIsAbsent fAbs $ do
              dfDflt <- defaultFor sr ComplexType
              withFresh sr envF ComplexType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $ do
               dzDflt <- pure (Const (Scalar ComplexType 1))
               withFresh sr envFD ComplexType dzDflt $ \envZ (dzName :: Proxy dzName) dzAbs -> recallIsAbsent dzAbs $ withEnvironment envZ $ do

                zpf'    <- findVarAtType sr zname       ComplexType envZ
                savePf' <- findVarAtType sr saveName    ComplexType envZ
                cpf'    <- findVarAtType sr counterName IntegerType envZ
                fPf     <- findVarAtType sr fName       ComplexType envZ
                dfPf    <- findVarAtType sr dfName      ComplexType envZ
                ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envZ
                spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envZ
                solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envZ

                let tracked0 = Map.singleton var (symbolVal dzName)

                    counterZ = Var counterName IntegerType cpf'
                    limitZ   = reindexValue envZ limit
                    tolZ     = reindexValue envZ tol
                    fVal     = Var fName  ComplexType fPf
                    dfVal    = Var dfName ComplexType dfPf

                    newtonStep = Set zpf' zname (Var zname ComplexType zpf' - fVal / dfVal)
                    converged  = Not (LTF tolZ (AbsC fVal))
                    c'         = And (Not converged) (counterZ `LTI` limitZ)
                    stuckCond  = Eql IntegerType counterZ limitZ
                    nan        = 0/0 :: Double
                    nanSolution = Const (Scalar ComplexType (nan :+ nan))

                    doSplice = spliceCompoundDualWirtinger sr var (symbolVal dzName) tracked0 cf args ComplexType envZ $
                      \envI f f' -> do
                        fPfI  <- findVarAtType sr fName  ComplexType envI
                        dfPfI <- findVarAtType sr dfName ComplexType envI
                        pure (Block [ Set fPfI fName f, Set dfPfI dfName f' ])

                initSplice <- doSplice
                stepSplice <- doSplice
                let b' = Block [ newtonStep, Set cpf' counterName (counterZ + 1), stepSplice ]

                pure $ Block
                  [ initSplice
                  , IfThenElse c' (DoWhile c' b') NoOp
                  , Set ipf   (Proxy @InternalIterations) counterZ
                  , Set spf   (Proxy @InternalStuck)      stuckCond
                  -- Publish the root z (not F(z), which is ~0 at convergence
                  -- by definition -- the root is what the caller wants).
                  , Set solpf (Proxy @InternalSolution)
                      (withEnvironment envZ $ ITE ComplexType stuckCond nanSolution (Var zname ComplexType zpf'))
                  , Set zpf'  zname (Var saveName ComplexType savePf')
                  ]

          RealType -> do
            fDflt <- defaultFor sr RealType
            withFresh sr env'' RealType fDflt $ \envF (fName :: Proxy fName) fAbs -> recallIsAbsent fAbs $ do
              dfDflt <- defaultFor sr RealType
              withFresh sr envF RealType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $ do
               dzDflt <- pure (Const (Scalar RealType 1))
               withFresh sr envFD RealType dzDflt $ \envZ (dzName :: Proxy dzName) dzAbs -> recallIsAbsent dzAbs $ withEnvironment envZ $ do

                zpf'    <- findVarAtType sr zname       RealType envZ
                savePf' <- findVarAtType sr saveName    RealType envZ
                cpf'    <- findVarAtType sr counterName IntegerType envZ
                fPf     <- findVarAtType sr fName       RealType envZ
                dfPf    <- findVarAtType sr dfName      RealType envZ
                ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envZ
                spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envZ
                solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envZ

                let tracked0 = Map.singleton var (symbolVal dzName)

                    counterZ = Var counterName IntegerType cpf'
                    limitZ   = reindexValue envZ limit
                    tolZ     = reindexValue envZ tol
                    fVal     = Var fName  RealType fPf
                    dfVal    = Var dfName RealType dfPf

                    newtonStep = Set zpf' zname (Var zname RealType zpf' - fVal / dfVal)
                    converged  = Not (LTF tolZ (AbsF fVal))
                    c'         = And (Not converged) (counterZ `LTI` limitZ)
                    stuckCond  = Eql IntegerType counterZ limitZ
                    nan        = 0/0 :: Double
                    nanSolution = Const (Scalar ComplexType (nan :+ nan))

                    doSplice = spliceCompoundDual sr var (symbolVal dzName) tracked0 cf args RealType envZ $
                      \envI f f' -> do
                        fPfI  <- findVarAtType sr fName  RealType envI
                        dfPfI <- findVarAtType sr dfName RealType envI
                        pure (Block [ Set fPfI fName f, Set dfPfI dfName f' ])

                initSplice <- doSplice
                stepSplice <- doSplice
                let b' = Block [ newtonStep, Set cpf' counterName (counterZ + 1), stepSplice ]

                pure $ Block
                  [ initSplice
                  , IfThenElse c' (DoWhile c' b') NoOp
                  , Set ipf   (Proxy @InternalIterations) counterZ
                  , Set spf   (Proxy @InternalStuck)      stuckCond
                  -- Publish the root z (not F(z), which is ~0 at convergence
                  -- by definition -- the root is what the caller wants).
                  , Set solpf (Proxy @InternalSolution)
                      (withEnvironment envZ $ ITE ComplexType stuckCond nanSolution (R2C (Var zname RealType zpf')))
                  , Set zpf'  zname (Var saveName RealType savePf')
                  ]

          _ -> throwError (Advice sr ("`solve` needs a real or complex unknown, but `"
                 ++ var ++ "` is " ++ showType zty ++ "."))

-- | @critical z -> f(args)@ where @f@ is a compound (looped) function:
-- Newton on the gradient (@critical@ is "solve on the gradient" -- see
-- 'Language.Code.Typecheck.tcCritical' for the closed-form case), but a
-- looped @f@'s gradient @g = dF/dz@ and @g@'s own derivative @g' = d²F/dz²@
-- both have to come from differentiating a loop.
--
-- @g@ is exactly what 'spliceCompoundDual' already produces as @F'@ -- one
-- splice gives @(F, g)@. @g'@ needs differentiating @g@ itself, and @g@ is
-- computed by a loop (whatever loop @f@'s body has), so the *same* trick
-- applies one level up: run the entire first splice again through
-- 'dualizeCode', seeded with a second, independent shadow of @z@, tracking
-- the outer variable that received @g@ so its shadow after this second
-- pass is @g'@. This only works because shadow names are always freshly
-- generated (see the module haddock) -- the second pass walks straight
-- through everything the first pass built (including the first pass's own
-- shadow variables) without colliding with any of it, no matter how deep.
--
-- Same materialization/re-splicing story as 'tcSolveCompound' (@g@/@g'@
-- have to be fresh at the current @z@ on every convergence check, so the
-- whole double-splice below runs once before the loop and once per
-- iteration), and the same @solve … continuing seed@ limitation.
tcCriticalCompound :: String
                   -> (CompoundFunction, [ParsedValue])
                   -> Maybe ParsedValue
                   -> Maybe ParsedValue
                   -> Bool
                   -> CheckedCode
tcCriticalCompound var (cf, args) mtol mlimit continuing sr (env :: EnvironmentProxy env)
  | continuing = throwError (Advice sr
      "`critical … continuing seed` is not yet supported for a looped (compound-function) equation.")
  | otherwise = do

    SomeSymbol zname <- pure (someSymbolVal var)
    FoundVar (zty :: TypeProxy zty) zpfEnv <- findVar sr zname env

    withFresh sr env zty (Var zname zty zpfEnv) $ \envS (saveName :: Proxy saveName) pfS -> recallIsAbsent pfS $
     withFresh sr envS IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

      limitValue <- case mlimit of
        Just (ParsedValue _ limitFun) -> limitFun IntegerType
        Nothing -> tcVar internalIterationLimit sr IntegerType

      withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

        let limit ::
              Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': '(saveName, zty) ': env, 'IntegerT)
            limit = Var limitName IntegerType (bindName limitName IntegerType pf')

        (tol :: Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': '(saveName, zty) ': env, 'RealT)) <-
          case mtol of
            Just pv -> atType pv RealType
            Nothing -> pure (Const (Scalar RealType solveTolerance))

        case zty of

          ComplexType -> do
            fDflt <- defaultFor sr ComplexType
            withFresh sr env'' ComplexType fDflt $ \envF (fName :: Proxy fName) fAbs -> recallIsAbsent fAbs $ do
             dfDflt <- defaultFor sr ComplexType
             withFresh sr envF ComplexType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $ do
              ddfDflt <- defaultFor sr ComplexType
              withFresh sr envFD ComplexType ddfDflt $ \(envFDD :: EnvironmentProxy envC) (ddfName :: Proxy ddfName) ddfAbs -> recallIsAbsent ddfAbs $ withEnvironment envFDD $ do

               zpf'    <- findVarAtType sr zname       ComplexType envFDD
               savePf' <- findVarAtType sr saveName    ComplexType envFDD
               cpf'    <- findVarAtType sr counterName IntegerType envFDD
               dfPf    <- findVarAtType sr dfName      ComplexType envFDD
               ddfPf   <- findVarAtType sr ddfName     ComplexType envFDD
               ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envFDD
               spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envFDD
               solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envFDD

               let counterZ = Var counterName IntegerType cpf'
                   limitZ   = reindexValue envFDD limit
                   tolZ     = reindexValue envFDD tol
                   gVal     = Var dfName  ComplexType dfPf   -- g  = F'
                   gPrimeVal = Var ddfName ComplexType ddfPf -- g' = F''

                   newtonStep = Set zpf' zname (Var zname ComplexType zpf' - gVal / gPrimeVal)
                   converged  = Not (LTF tolZ (AbsC gVal))
                   c'         = And (Not converged) (counterZ `LTI` limitZ)
                   stuckCond  = Eql IntegerType counterZ limitZ
                   nan        = 0/0 :: Double
                   nanSolution = Const (Scalar ComplexType (nan :+ nan))

                   -- Splice f(z) once (seeded with a fresh shadow of z) to
                   -- get (F, g) and copy them into the outer fName/dfName;
                   -- then splice the *entire resulting program* again
                   -- (seeded with a second, independent fresh shadow of z,
                   -- tracking dfName -> ddfName) so ddfName ends up holding
                   -- g's own derivative, g'.
                   -- `withFresh`'s freshness is relative to the *apparent*
                   -- length of the environment it's given -- it can't see
                   -- names hidden inside an already-built nested Let-chain.
                   -- So both fresh seeds (dz1, dz2) must be declared BEFORE
                   -- `fos` is built, extending the base environment `fos`'s
                   -- own internal fresh names are generated from; declaring
                   -- dz2 afterwards, from the same starting environment fos
                   -- itself started from, risks it colliding with one of
                   -- fos's own internal names (this happened once).
                   doDoubleSplice :: TC (Code envC)
                   doDoubleSplice = do
                     dz1Dflt <- pure (Const (Scalar ComplexType 1))
                     withFresh sr envFDD ComplexType dz1Dflt $ \envZ1 (dz1 :: Proxy dz1) dz1Abs -> recallIsAbsent dz1Abs $ do
                       dz2Dflt <- pure (Const (Scalar ComplexType 1))
                       withFresh sr envZ1 ComplexType dz2Dflt $ \envZ2 (dz2 :: Proxy dz2) dz2Abs -> recallIsAbsent dz2Abs $ do
                         fos <- spliceCompoundDualWirtinger sr var (symbolVal dz1) (Map.singleton var (symbolVal dz1)) cf args ComplexType envZ2 $
                           \envI f f' -> do
                             fPfI  <- findVarAtType sr fName  ComplexType envI
                             dfPfI <- findVarAtType sr dfName ComplexType envI
                             pure (Block [ Set fPfI fName f, Set dfPfI dfName f' ])
                         dualizeCodeWirtinger sr var (symbolVal dz2)
                           (Map.fromList [ (var, symbolVal dz2), (symbolVal dfName, symbolVal ddfName) ])
                           fos

               initSplice <- doDoubleSplice
               stepSplice <- doDoubleSplice
               let b' = Block [ newtonStep, Set cpf' counterName (counterZ + 1), stepSplice ]

               pure $ Block
                 [ initSplice
                 , IfThenElse c' (DoWhile c' b') NoOp
                 , Set ipf   (Proxy @InternalIterations) counterZ
                 , Set spf   (Proxy @InternalStuck)      stuckCond
                 -- Publish the critical point z (not g(z), which is ~0 at
                 -- convergence by definition).
                 , Set solpf (Proxy @InternalSolution)
                     (withEnvironment envFDD $ ITE ComplexType stuckCond nanSolution (Var zname ComplexType zpf'))
                 , Set zpf'  zname (Var saveName ComplexType savePf')
                 ]

          RealType -> do
            fDflt <- defaultFor sr RealType
            withFresh sr env'' RealType fDflt $ \envF (fName :: Proxy fName) fAbs -> recallIsAbsent fAbs $ do
             dfDflt <- defaultFor sr RealType
             withFresh sr envF RealType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $ do
              ddfDflt <- defaultFor sr RealType
              withFresh sr envFD RealType ddfDflt $ \(envFDD :: EnvironmentProxy envC) (ddfName :: Proxy ddfName) ddfAbs -> recallIsAbsent ddfAbs $ withEnvironment envFDD $ do

               zpf'    <- findVarAtType sr zname       RealType envFDD
               savePf' <- findVarAtType sr saveName    RealType envFDD
               cpf'    <- findVarAtType sr counterName IntegerType envFDD
               dfPf    <- findVarAtType sr dfName      RealType envFDD
               ddfPf   <- findVarAtType sr ddfName     RealType envFDD
               ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envFDD
               spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envFDD
               solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envFDD

               let counterZ = Var counterName IntegerType cpf'
                   limitZ   = reindexValue envFDD limit
                   tolZ     = reindexValue envFDD tol
                   gVal     = Var dfName  RealType dfPf
                   gPrimeVal = Var ddfName RealType ddfPf

                   newtonStep = Set zpf' zname (Var zname RealType zpf' - gVal / gPrimeVal)
                   converged  = Not (LTF tolZ (AbsF gVal))
                   c'         = And (Not converged) (counterZ `LTI` limitZ)
                   stuckCond  = Eql IntegerType counterZ limitZ
                   nan        = 0/0 :: Double
                   nanSolution = Const (Scalar ComplexType (nan :+ nan))

                   -- See the ComplexType branch's comment: both fresh seeds
                   -- must be declared before `fos` is built.
                   doDoubleSplice :: TC (Code envC)
                   doDoubleSplice = do
                     dz1Dflt <- pure (Const (Scalar RealType 1))
                     withFresh sr envFDD RealType dz1Dflt $ \envZ1 (dz1 :: Proxy dz1) dz1Abs -> recallIsAbsent dz1Abs $ do
                       dz2Dflt <- pure (Const (Scalar RealType 1))
                       withFresh sr envZ1 RealType dz2Dflt $ \envZ2 (dz2 :: Proxy dz2) dz2Abs -> recallIsAbsent dz2Abs $ do
                         fos <- spliceCompoundDual sr var (symbolVal dz1) (Map.singleton var (symbolVal dz1)) cf args RealType envZ2 $
                           \envI f f' -> do
                             fPfI  <- findVarAtType sr fName  RealType envI
                             dfPfI <- findVarAtType sr dfName RealType envI
                             pure (Block [ Set fPfI fName f, Set dfPfI dfName f' ])
                         dualizeCode sr var (symbolVal dz2)
                           (Map.fromList [ (var, symbolVal dz2), (symbolVal dfName, symbolVal ddfName) ])
                           fos

               initSplice <- doDoubleSplice
               stepSplice <- doDoubleSplice
               let b' = Block [ newtonStep, Set cpf' counterName (counterZ + 1), stepSplice ]

               pure $ Block
                 [ initSplice
                 , IfThenElse c' (DoWhile c' b') NoOp
                 , Set ipf   (Proxy @InternalIterations) counterZ
                 , Set spf   (Proxy @InternalStuck)      stuckCond
                 -- Publish the critical point z (not g(z), which is ~0 at
                 -- convergence by definition).
                 , Set solpf (Proxy @InternalSolution)
                     (withEnvironment envFDD $ ITE ComplexType stuckCond nanSolution (R2C (Var zname RealType zpf')))
                 , Set zpf'  zname (Var saveName RealType savePf')
                 ]

          _ -> throwError (Advice sr ("`critical` needs a real or complex unknown, but `"
                 ++ var ++ "` is " ++ showType zty ++ "."))
