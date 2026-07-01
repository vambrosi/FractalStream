{-# language AllowAmbiguousTypes #-}

-- | Building blocks for differentiating a value through a /tracked/
-- variable's shadow, rather than a single fixed target.
--
-- A tracked variable named @v@'s derivative lives in another, genuinely
-- separate runtime variable, @shadowName v@, in the same scope (not a
-- symbolic expression -- the dual-number transform over looped 'Code'
-- (forthcoming) needs this, since the loop body that updates @v@ runs an
-- unknown number of times). 'dValue' differentiates a single (non-looped)
-- 'Value' under this convention.
module Language.Code.Dual
  ( shadowName
  , Tracked
  , dValue
  , dualizeCode
  , spliceCompoundDual
  , tcSolveCompound
  ) where

import FractalStream.Prelude
import Language.Value
import Language.Value.Typecheck
  ( ParsedValue(..), atType, tcVar, internalIterationLimit
  , InternalIterations, InternalStuck, InternalSolution )
import Language.Value.Derivative (derivativeWith)
import Language.Value.Reindex (reindexValue)
import Language.Code
import Language.Code.Reindex (reindexCode)
import Language.Code.Typecheck
  ( CompoundFunction(..), letBind, atEnv, checkPure, defaultFor, inferArgType
  , withFresh, solveTolerance, CheckedCode )
import Language.Typecheck
import Language.Parser.SourceRange
import qualified Data.Set as Set

-- | The names of variables currently tracked for differentiation.
type Tracked = Set String

-- | The (bracketed, collision-proof) name of a tracked variable's shadow.
-- User identifiers can't contain brackets or spaces, so this can never
-- collide with a user-written name.
shadowName :: String -> String
shadowName v = "[dual] " ++ v

-- | Differentiate @v@ with respect to the seed direction, using @tracked@'s
-- shadow convention: a tracked variable's derivative is read from its
-- shadow; anything else (an untracked variable, a constant, ...) is
-- locally constant (0). @blame@ only fills the "with respect to ..." slot
-- of a 'DiffNotImplemented' error (e.g. when @v@ contains a loop or an
-- unsupported node).
dValue :: String -> Tracked -> SourceRange -> Value et -> TC (Value et)
dValue blame tracked sr v = derivativeWith blame shadowOf sr v
  where
    shadowOf :: forall et'. Value et' -> Maybe (Value et')
    shadowOf (Var name ty _)
      | symbolVal name `Set.member` tracked
      = case someSymbolVal (shadowName (symbolVal name)) of
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
-- variable named in @tracked@ (extended as new @Let@s are discovered) gets
-- a shadow (see 'shadowName') holding its running derivative with respect
-- to the seed direction. A tracked variable must already have its shadow
-- declared (with its initial derivative) in the environment @code@ starts
-- in; @dualizeCode@ only introduces shadows for variables @code@ itself
-- declares via @Let@.
--
-- @blame@ only fills the "with respect to ..." slot of a
-- 'DiffNotImplemented' error. Only @Let@/@Set@/@Block@/@IfThenElse@/
-- @DoWhile@/@NoOp@ are supported; @ForEach@/@Lookup@/@DrawCommand@ are
-- rejected with a clear error (not used by potential-style numeric loop
-- bodies).
dualizeCode :: SourceRange -> String -> Tracked -> Code env -> TC (Code env)
dualizeCode sr blame tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        dv <- dValue blame tracked sr v
        letBind sr (symbolVal name) ty v env $ \env1 ->
          letBind sr (shadowName (symbolVal name)) ty (reindexValue env1 dv) env1 $ \env2 ->
            dualizeCode sr blame (Set.insert (symbolVal name) tracked) (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCode sr blame tracked body

  Set pf name v
    | symbolVal name `Set.member` tracked, isDifferentiable (typeOfValue v) -> do
        let ty = typeOfValue v
        dv <- dValue blame tracked sr v
        case someSymbolVal (shadowName (symbolVal name)) of
          SomeSymbol sname -> case lookupEnv sname ty (envProxy Proxy) of
            -- `dv` (and `v`, if `v` reads `name` itself, e.g. `w <- w * x`)
            -- is evaluated against `name`'s OLD value, so the shadow must
            -- be updated before `name` itself is overwritten below.
            Found spf -> pure (Block [ Set spf sname dv, Set pf name v ])
            _ -> throwError (Advice sr
                   ("dualizeCode: internal error, `" ++ symbolVal name
                     ++ "` is tracked but its shadow is missing."))
    | otherwise -> pure (Set pf name v)

  Block stmts -> Block <$> traverse (dualizeCode sr blame tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCode sr blame tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCode sr blame tracked yes <*> dualizeCode sr blame tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCode: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCode: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCode: list operations are not supported.")

-- | Bind each argument to a compound function call to a fresh name (as
-- 'Language.Code.Typecheck.spliceArgs' does for an ordinary, non-dual
-- call), and additionally bind its derivative (with respect to @tracked0@,
-- fixed across all arguments -- an argument expression can't reference
-- another argument's fresh name, since all arguments are evaluated in the
-- same outer scope) to a shadow right alongside it. The continuation's
-- 'Tracked' is @tracked0@ plus every differentiable argument's fresh name.
spliceArgsDual
  :: forall env
   . SourceRange
  -> String
  -> Tracked
  -> [(String, Maybe SomeType, ParsedValue)]
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> TC (Code env'))
  -> TC (Code env)
spliceArgsDual _  _     tracked0 [] env k = withEnvironment env (k env tracked0)
spliceArgsDual sr blame tracked0 ((fresh, ann, arg) : rest) env k = withEnvironment env $ do
  SomeType (pty :: TypeProxy pty) <- inferArgType sr ann arg env
  withKnownType pty $ do
    argVal <- atType arg pty :: TC (Value '(env, pty))
    dArg   <- dValue blame tracked0 sr argVal
    letBind sr fresh pty argVal env $ \env1 -> case pty of
      ComplexType ->
        letBind sr (shadowName fresh) pty (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDual sr blame tracked0 rest env2
            (\envF trackedF -> k envF (Set.insert fresh trackedF))
      RealType ->
        letBind sr (shadowName fresh) pty (reindexValue env1 dArg) env1 $ \env2 ->
          spliceArgsDual sr blame tracked0 rest env2
            (\envF trackedF -> k envF (Set.insert fresh trackedF))
      _ -> spliceArgsDual sr blame tracked0 rest env1 k

-- | Splice a compound function call, differentiated with respect to
-- whichever variables are already tracked in @env@ (their shadows must
-- already be declared there -- e.g. the Newton unknown, seeded to 1),
-- producing @(F, F')@ as materialized (@Set@-mutated) local variables of
-- type @ty@. @F@/@F'@ are handed to a continuation rather than copied into
-- a target variable, unlike 'Language.Code.Typecheck.tcSetCompound' (which
-- this otherwise mirrors) -- a Newton loop needs them as ordinary
-- expressions, and they have to be materialized (not symbolic) because the
-- function body may contain a loop.
spliceCompoundDual
  :: SourceRange
  -> String
  -> Tracked
  -> CompoundFunction
  -> [ParsedValue]
  -> TypeProxy ty
  -> EnvironmentProxy env
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Value '(env', ty) -> Value '(env', ty) -> TC (Code env'))
  -> TC (Code env)
spliceCompoundDual sr blame tracked0 cf args ty env k
  | length args /= length (cfParams cf) =
      throwError (Advice sr ("The function " ++ cfName cf ++ " expects "
        ++ show (length (cfParams cf)) ++ " argument(s), but "
        ++ show (length args) ++ " were given."))
  | otherwise = withEnvironment env $
      spliceArgsDual sr blame tracked0
        (zip3 (cfFreshParams cf) (map snd (cfParams cf)) args) env $ \envP tracked ->
        withKnownType ty $ do
          dflt <- defaultFor sr ty
          letBind sr (cfResultName cf) ty dflt envP $ \envR -> withEnvironment envR $ do
            dfltShadow <- defaultFor sr ty
            letBind sr (shadowName (cfResultName cf)) ty dfltShadow envR $ \envRD -> withEnvironment envRD $ do
              body  <- atEnv envRD (cfBody cf)
              checkPure cf sr body
              dbody <- dualizeCode sr blame (Set.insert (cfResultName cf) tracked) body
              case (someSymbolVal (cfResultName cf), someSymbolVal (shadowName (cfResultName cf))) of
                (SomeSymbol res, SomeSymbol dres) -> do
                  resPf  <- findVarAtType sr res  ty envRD
                  dresPf <- findVarAtType sr dres ty envRD
                  restCode <- k envRD (Var res ty resPf) (Var dres ty dresPf)
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
              withFresh sr envF ComplexType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $
               letBind sr (shadowName var) ComplexType (Const (Scalar ComplexType 1)) envFD $ \envZ -> withEnvironment envZ $ do

                zpf'    <- findVarAtType sr zname       ComplexType envZ
                savePf' <- findVarAtType sr saveName    ComplexType envZ
                cpf'    <- findVarAtType sr counterName IntegerType envZ
                fPf     <- findVarAtType sr fName       ComplexType envZ
                dfPf    <- findVarAtType sr dfName      ComplexType envZ
                ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envZ
                spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envZ
                solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envZ

                let counterZ = Var counterName IntegerType cpf'
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

                    doSplice = spliceCompoundDual sr var (Set.singleton var) cf args ComplexType envZ $
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
              withFresh sr envF RealType dfDflt $ \envFD (dfName :: Proxy dfName) dfAbs -> recallIsAbsent dfAbs $
               letBind sr (shadowName var) RealType (Const (Scalar RealType 1)) envFD $ \envZ -> withEnvironment envZ $ do

                zpf'    <- findVarAtType sr zname       RealType envZ
                savePf' <- findVarAtType sr saveName    RealType envZ
                cpf'    <- findVarAtType sr counterName IntegerType envZ
                fPf     <- findVarAtType sr fName       RealType envZ
                dfPf    <- findVarAtType sr dfName      RealType envZ
                ipf     <- findVarAtType sr (Proxy @InternalIterations) IntegerType envZ
                spf     <- findVarAtType sr (Proxy @InternalStuck)      BooleanType envZ
                solpf   <- findVarAtType sr (Proxy @InternalSolution)   ComplexType envZ

                let counterZ = Var counterName IntegerType cpf'
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

                    doSplice = spliceCompoundDual sr var (Set.singleton var) cf args RealType envZ $
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
