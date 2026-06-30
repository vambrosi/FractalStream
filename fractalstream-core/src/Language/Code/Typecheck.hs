module Language.Code.Typecheck where

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Typecheck
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Parser.SourceRange
import Language.Value.Typecheck (tcVar, internalIterationLimit, InternalIterations, InternalStuck, InternalSolution, InternalContSeed, InternalHasSeed)
import Language.Value.Derivative (derivative)

import Data.Color (black)
import Data.Indexed.Functor (indexedFoldM)
import qualified Data.Set as Set

------------------------------------------------------
-- Parsed code
------------------------------------------------------

-- | Parse code, which has not yet been checked for
-- type correctness, scope correctness, etc
newtype ParsedCode = ParsedCode
  (forall env. EnvironmentProxy env -> TC (Code env))

-- | Code which has been checked for type & scope
-- correctness.
type CheckedCode = SourceRange -> (forall env. KnownEnvironment env => EnvironmentProxy env -> TC (Code env))

atEnv :: EnvironmentProxy env -> ParsedCode -> TC (Code env)
atEnv env (ParsedCode f) = f env

------------------------------------------------------
-- Code environment checking
------------------------------------------------------

tcBlock :: [ParsedCode] -> CheckedCode
tcBlock body _sr env = Block <$> traverse (atEnv env) body

tcLet :: String -> FSType -> ParsedValue -> ParsedCode -> CheckedCode
tcLet n t v c sr env = withType t $ \ty -> do
  SomeSymbol name <- pure (someSymbolVal n)
  DeclaredVar _ env' <- declareVar sr name ty env
  Let bindingEvidence name <$> atType v ty <*> atEnv env' c

tcSet :: String -> ParsedValue -> CheckedCode
tcSet n v sr env = do
  SomeSymbol name <- pure (someSymbolVal n)
  FoundVar ty pf <- findVar sr name env
  Set pf name <$> atType v ty

tcGenericLoop :: Bool
              -> (forall env. KnownEnvironment env =>
                    Value '(env, 'BooleanT) -> Code env -> Code env)
              -> Maybe ParsedValue -> ParsedCode -> ParsedValue -> CheckedCode
tcGenericLoop negateCondition mkLoop mlimit body cond sr (env :: EnvironmentProxy env) =
  withFresh sr env  IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

  limitValue <- case mlimit of
    Just (ParsedValue _ limitFun) -> limitFun IntegerType
    Nothing -> tcVar internalIterationLimit sr IntegerType

  withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

    pf <- findVarAtType sr counterName IntegerType env''
    let counter, limit ::
          Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': env, 'IntegerT)
        counter = Var counterName IntegerType pf
        limit =   Var limitName IntegerType   (bindName limitName IntegerType pf')

    c <- atType cond BooleanType
    b <- atEnv env'' body
    let b' = Block [ b, Set pf counterName (counter + 1)]
        c' = And (if negateCondition then Not c else c) (counter `LTI` limit)
        iterations = Proxy @InternalIterations
        stuck      = Proxy @InternalStuck
    ipf <- findVarAtType sr iterations IntegerType env''
    spf <- findVarAtType sr stuck      BooleanType env''
    pure $ Block
      [ mkLoop c' b'
      , Set ipf iterations counter
      , Set spf stuck (Eql IntegerType counter limit) ]

tcWhile, tcUntil :: ParsedValue -> Maybe ParsedValue -> ParsedCode -> CheckedCode
tcDoWhile, tcDoUntil :: Maybe ParsedValue -> ParsedCode -> ParsedValue -> CheckedCode

tcWhile c l b =
  tcGenericLoop False (\cond body -> IfThenElse cond (DoWhile cond body) NoOp) l b c
tcUntil c l b =
  tcGenericLoop True  (\cond body -> IfThenElse cond (DoWhile cond body) NoOp) l b c
tcDoWhile = tcGenericLoop False DoWhile
tcDoUntil = tcGenericLoop True  DoWhile

tcIfThenElse :: ParsedValue -> ParsedCode -> ParsedCode -> CheckedCode
tcIfThenElse cond yes no _sr env =
  IfThenElse <$> atType cond BooleanType
             <*> atEnv env yes
             <*> atEnv env no

tcIterate :: String -> ParsedValue -> Bool -> ParsedValue -> Maybe ParsedValue -> CheckedCode
tcIterate var expr isWhile cond upto sr env = do
  let body = ParsedCode (\e -> withEnvironment e $ tcSet var expr sr e)
      tc = if isWhile then tcWhile else tcUntil
  tc cond upto body sr env

------------------------------------------------------
-- solve / preimage (Newton root-finding statements)
------------------------------------------------------

-- | Default convergence tolerance on @|F|@ when no @within@ clause is given.
solveTolerance :: Double
solveTolerance = 1e-10

-- | @solve z -> F@: find a root of @F = 0@ by a counted Newton iteration
-- seeded from @z@'s current value, and store it in the internal @solution@
-- variable. The unknown @z@ itself is left unchanged (it stays the seed /
-- coordinate), mirroring how @stuck@/@iterations@ report a loop's outcome
-- without disturbing its inputs.
--
-- Lowers to the same loop shape as 'tcIterate' (fresh counter + limit,
-- @iterations@/@stuck@ bookkeeping), with a fixed body (the Newton step
-- @z <- z - F/F'@) and exit condition (@|F| <= tol@). Because @F@ is written in
-- terms of @z@, the iteration runs on @z@ and the original value is saved
-- first and restored afterwards. @F'@ is obtained symbolically via 'derivative'
-- w.r.t. @Var z@, so @F@ must be a differentiable closed-form expression.
-- @solution@ is complex; for a real unknown the (real) root is widened with
-- @R2C@. @stuck@ is true iff Newton did not converge within the budget.
tcSolve :: String              -- ^ the unknown variable's name
        -> ParsedValue         -- ^ the equation body @F@
        -> Maybe ParsedValue   -- ^ optional tolerance (@within@ clause)
        -> Maybe ParsedValue   -- ^ optional iteration limit (@up to N times@)
        -> Bool                -- ^ @continuing seed@: block-seed from a coarse field
        -> CheckedCode
-- When @continuing@ is set (`solve … continuing seed`), the unknown is seeded
-- from the engine's continuation field rather than its per-pixel value: the
-- lowering reads the incoming seed from @[internal] continuation seed@ (when
-- @[internal] continuation has seed@ is true; otherwise it keeps the unknown's
-- current value as the cold-start anchor) and writes the result back into the
-- same variable so the field captures the cell's solution. The pre-pass and the
-- per-pixel render fill those variables (RW3–RW5).
tcSolve var pF mtol mlimit continuing sr (env :: EnvironmentProxy env) = do

  SomeSymbol zname <- pure (someSymbolVal var)
  FoundVar (zty :: TypeProxy zty) zpfEnv <- findVar sr zname env

  -- Save the unknown's current value so we can restore it after solving.
  withFresh sr env zty (Var zname zty zpfEnv) $ \envS (saveName :: Proxy saveName) pfS -> recallIsAbsent pfS $
   withFresh sr envS IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

    limitValue <- case mlimit of
      Just (ParsedValue _ limitFun) -> limitFun IntegerType
      Nothing -> tcVar internalIterationLimit sr IntegerType

    withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

      pf <- findVarAtType sr counterName IntegerType env''
      let counter, limit ::
            Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': '(saveName, zty) ': env, 'IntegerT)
          counter = Var counterName IntegerType pf
          limit =   Var limitName IntegerType   (bindName limitName IntegerType pf')

      zpf    <- findVarAtType sr zname    zty         env''
      savePf <- findVarAtType sr saveName zty         env''

      -- The Newton step, the absolute residual |F|, and the (complex) value to
      -- store as `solution`, built at the unknown's type (Real or Complex).
      -- Both halves use the overloaded Num/Fractional instances on Value, so
      -- the body is identical apart from Abs/R2C and the type.
      (newtonStep, absF, solutionVal) <- case zty of
        ComplexType -> do
          f  <- atType pF ComplexType
          f' <- diffClosedForm sr var (Var zname ComplexType zpf) f
          pure ( Set zpf zname (Var zname ComplexType zpf - f / f')
               , AbsC f
               , Var zname ComplexType zpf )
        RealType -> do
          f  <- atType pF RealType
          f' <- diffClosedForm sr var (Var zname RealType zpf) f
          pure ( Set zpf zname (Var zname RealType zpf - f / f')
               , AbsF f
               , R2C (Var zname RealType zpf) )
        _ -> throwError (Advice sr ("`solve`/`preimage` needs a real or complex unknown, but `"
               ++ var ++ "` is " ++ showType zty ++ "."))

      tol <- case mtol of
        Just pv -> atType pv RealType
        Nothing -> pure (Const (Scalar RealType solveTolerance))

      let converged = Not (LTF tol absF)              -- |F| <= tol
          c' = And (Not converged) (counter `LTI` limit)
          b' = Block [ newtonStep, Set pf counterName (counter + 1) ]
          iterations = Proxy @InternalIterations
          stuck      = Proxy @InternalStuck
          solution   = Proxy @InternalSolution
          stuckCond  = Eql IntegerType counter limit   -- hit the budget => didn't converge
          nan        = 0/0 :: Double
          nanSolution = Const (Scalar ComplexType (nan :+ nan))
      ipf  <- findVarAtType sr iterations IntegerType env''
      spf  <- findVarAtType sr stuck      BooleanType env''
      solpf <- findVarAtType sr solution  ComplexType env''

      -- For a `continuing` solve: seed the unknown from the continuation field
      -- variable (prefix) when a seed is available, and capture the result back
      -- into it (suffix) so the field stores this cell's solution.
      (contPrefix, contSuffix) <- if continuing
        then case zty of
          ComplexType -> do
            cpf <- findVarAtType sr (Proxy @InternalContSeed) ComplexType env''
            hpf <- findVarAtType sr (Proxy @InternalHasSeed)  BooleanType env''
            let seedInject =
                  IfThenElse (Var (Proxy @InternalHasSeed) BooleanType hpf)
                    (Set zpf zname (Var (Proxy @InternalContSeed) ComplexType cpf))
                    NoOp
                captureSol =
                  Set cpf (Proxy @InternalContSeed)
                    (Var solution ComplexType solpf)
            pure ([seedInject], [captureSol])
          _ -> throwError (Advice sr
                 "`solve … continuing seed` currently supports a complex unknown only.")
        else pure ([], [])

      pure $ Block $ contPrefix ++
        [ IfThenElse c' (DoWhile c' b') NoOp
        , Set ipf  iterations counter
        , Set spf  stuck      stuckCond
        -- Publish the root, or NaN if Newton did not converge, so a failure
        -- propagates into anything that reads `solution` instead of leaving a
        -- plausible-looking last iterate.
        , Set solpf solution
            (withEnvironment env'' $ ITE ComplexType stuckCond nanSolution solutionVal)
        , Set zpf  zname      (Var saveName zty savePf) ]  -- restore the unknown
        ++ contSuffix

-- | Differentiate a closed-form equation body, turning the internal
-- 'DiffNotImplemented' (thrown on loops / unsupported nodes) into a clear
-- user-facing error: this is the boundary with the future non-closed-form work.
diffClosedForm :: SourceRange -> String -> Value et -> Value et -> TC (Value et)
diffClosedForm sr var z f = catchError (derivative sr z sr f) $ \case
  DiffNotImplemented{} -> throwError (Advice sr
    ("`solve`/`preimage` needs a differentiable closed-form equation, but the body for `"
     ++ var ++ "` contains a loop or an unsupported construct."))
  err -> throwError err

-- | @preimage z -> F of v@: find a solution of @F = v@ near @z@'s current
-- value, leaving @z@ unchanged and publishing the result in @solution@. Pure
-- sugar for @solve z -> F - v@ (same machinery; @v@ is
-- constant w.r.t. @z@, so @F'@ is unchanged).
tcPreimage :: String -> ParsedValue -> ParsedValue
           -> Maybe ParsedValue -> Maybe ParsedValue -> Bool -> CheckedCode
tcPreimage var pF pV = tcSolve var (subParsed pF pV)

-- | The 'ParsedValue' @F - v@ (real or complex), used to desugar @preimage@.
subParsed :: ParsedValue -> ParsedValue -> ParsedValue
subParsed pF@(ParsedValue sr _) pV = ParsedValue sr $ \case
  ComplexType -> (-) <$> atType pF ComplexType <*> atType pV ComplexType
  RealType    -> (-) <$> atType pF RealType    <*> atType pV RealType
  ty          -> throwError (Surprise sr "the body of `preimage`"
                   (an (SomeType ty)) (Expected "a real or complex number"))

tcPoint :: KnownEnvironment env => ParsedValue -> TC (Value '(env, 'Pair 'RealT 'RealT))
tcPoint p@(ParsedValue sr _) =
  tryEachType (Surprise sr "this"
               "not a complex number or pair of real numbers"
               (Expected "something point-like"))
    [ atType p (PairType RealType RealType)
    , C2R2 <$> atType p ComplexType ]

tcDrawPoint :: ParsedValue -> CheckedCode
tcDrawPoint v _sr env = DrawCommand . DrawPoint env <$> tcPoint v

tcDrawCircle :: Bool -> ParsedValue -> ParsedValue -> CheckedCode
tcDrawCircle isFilled center radius _sr env =
  DrawCommand <$> (DrawCircle env isFilled <$> atType radius RealType <*> tcPoint center)

tcDrawRect :: Bool -> ParsedValue -> ParsedValue -> CheckedCode
tcDrawRect isFilled ul lr _sr env =
  DrawCommand <$> (DrawRect env isFilled <$> tcPoint ul <*> tcPoint lr)

tcDrawLine :: ParsedValue -> ParsedValue -> CheckedCode
tcDrawLine ul lr _sr env =
  DrawCommand <$> (DrawLine env <$> tcPoint ul <*> tcPoint lr)

tcSetStroke :: ParsedValue -> CheckedCode
tcSetStroke c _sr env = DrawCommand . SetStroke env <$> atType c ColorType

tcSetFill :: ParsedValue -> CheckedCode
tcSetFill c _sr env = DrawCommand . SetFill env <$> atType c ColorType

tcClear :: CheckedCode
tcClear _sr env = pure (DrawCommand $ Clear env)

tcWrite :: ParsedValue -> ParsedValue -> CheckedCode
tcWrite txt pt _sr env = DrawCommand <$> (Write env <$> atType txt TextType <*> tcPoint pt)

tcListFor :: String -> String -> ParsedCode -> CheckedCode
tcListFor itemName listName body sr env = do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)

  ListExists itemTy pfListPresent <- getListType sr list env
  DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
  ForEach pfListPresent list (ListType itemTy) item pfItemAbsent env env'
    <$> atEnv env' body

tcListWith :: String
           -> ParsedValue
           -> String
           -> ParsedCode
           -> Maybe ParsedCode
           -> CheckedCode
tcListWith itemName predicate listName body fallback sr env = do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  ListExists itemTy pfListPresent <- getListType sr list env
  DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
  Lookup pfListPresent list (ListType itemTy) item pfItemAbsent env' env
    <$> atType predicate BooleanType
    <*> atEnv env' body
    <*> traverse (atEnv env) fallback

-----------------
-- Utilities
-----------------

data ListExists name env where
  ListExists :: forall name itemTy env
              . (KnownSymbol name, KnownType itemTy)
             => TypeProxy itemTy
             -> NameIsPresent name ('ListT itemTy) env
             -> ListExists name env

getListType :: KnownSymbol name
            => SourceRange
            -> Proxy name
            -> EnvironmentProxy env
            -> TC (ListExists name env)
getListType sr name env = case lookupEnv' name env of
  Absent'{} -> throwError (MissingName sr (symbolVal name))
  Found' ty pf -> case ty of
    ListType itemTy -> pure (ListExists itemTy pf)
    _ -> throwError (Surprise sr (symbolVal name) (an $ SomeType ty) (Expected "a list"))

withFresh :: forall ty env
           . SourceRange
          -> EnvironmentProxy env
          -> TypeProxy ty
          -> Value '(env, ty)
          -> (forall fresh. KnownSymbol fresh => EnvironmentProxy ( '(fresh, ty) ': env)
                                              -> Proxy fresh
                                              -> NameIsAbsent fresh env
                                              -> TC (Code ( '(fresh, ty) ': env)))
          -> TC (Code env)
withFresh sr env ty value action = withEnvironment env $ do
  let tmpName = "[internal] fresh #" ++
        show (length $ fromEnvironment env (\_ _ -> ()))
  case someSymbolVal tmpName of
    SomeSymbol tmp -> case lookupEnv tmp ty env of
      Absent pf -> recallIsAbsent pf $ let_ value <$>
        action (bindNameEnv tmp ty pf env) tmp pf
      _ -> throwError (Internal $ AlreadyDefined sr tmpName)

------------------------------------------------------
-- Compound (statement-bodied) user functions
------------------------------------------------------

-- | A user function whose body is a compound block of statements (locals,
-- loops, conditionals, reassignment) that delivers its result by assigning the
-- slot. Inlined by /splicing/ the statements at the call site; for now,
-- callable only in statement position (@target <- f(args)@).
data CompoundFunction = CompoundFunction
  { cfName        :: String
  , cfParams      :: [(String, Maybe SomeType)]
  , cfFreshParams :: [String]
  , cfResultName  :: String     -- ^ fresh name the result slot was renamed to
  , cfBody        :: ParsedCode
  }

-- | Typecheck @target <- f(args)@ for a compound function @f@: bind each
-- parameter to its argument with a @Let@, declare the result slot (initialised
-- to a default), run the (renamed) body, then copy the result into @target@.
tcSetCompound :: String -> CompoundFunction -> [ParsedValue] -> CheckedCode
tcSetCompound targetName cf args sr env
  | length args /= length (cfParams cf) =
      throwError (Advice sr ("The function " ++ cfName cf ++ " expects "
        ++ show (length (cfParams cf)) ++ " argument(s), but "
        ++ show (length args) ++ " were given."))
  | otherwise = withEnvironment env $ case someSymbolVal targetName of
      SomeSymbol target ->
        spliceArgs sr (zip3 (cfFreshParams cf) (map snd (cfParams cf)) args) env $ \envP -> do
          FoundVar rty _ <- findVar sr target envP
          withKnownType rty $ do
            dflt <- defaultFor sr rty
            letBind sr (cfResultName cf) rty dflt envP $ \envR -> withEnvironment envR $ do
              body  <- atEnv envR (cfBody cf)
              checkPure cf sr body
              tgtPf <- findVarAtType sr target rty envR
              case someSymbolVal (cfResultName cf) of
                SomeSymbol res -> do
                  resPf <- findVarAtType sr res rty envR
                  pure (Block [ body, Set tgtPf target (Var res rty resPf) ])

-- | Verify that a compound function body is pure: it may assign only its own
-- result slot, its parameters, and locals it declares. Assigning any other
-- (caller/config) variable is rejected. Internal bookkeeping names (loop
-- counters, @[internal] …@) are bracketed and always allowed.
checkPure :: CompoundFunction -> SourceRange -> Code env -> TC ()
checkPure cf sr body =
  case Set.toList illegal of
    []        -> pure ()
    (bad : _) -> throwError (Advice sr
      ("A function body may not modify `" ++ bad ++ "`; functions must be pure."))
  where
    (setVars, letVars) = fnBodyVars body
    allowed = Set.insert (cfResultName cf)
            $ Set.union (Set.fromList (cfFreshParams cf)) letVars
    illegal = Set.filter (\n -> take 1 n /= "[") (setVars `Set.difference` allowed)

-- | Collect the names a code block assigns to (via @Set@) and the names it
-- declares locally (via @Let@), at any depth.
fnBodyVars :: Code env -> (Set String, Set String)
fnBodyVars c = execState (indexedFoldM @Unit gather c) (Set.empty, Set.empty)
  where
    gather :: forall e. CodeF Unit e -> State (Set String, Set String) ()
    gather = \case
      Set _ name _   -> modify' (\(s, l) -> (Set.insert (symbolVal name) s, l))
      Let _ name _ _ -> modify' (\(s, l) -> (s, Set.insert (symbolVal name) l))
      _              -> pure ()

-- | Typecheck each argument in the call-site environment and bind it to the
-- corresponding fresh parameter name with a @Let@, threading the (growing)
-- environment to the continuation.
spliceArgs :: forall env
            . SourceRange
           -> [(String, Maybe SomeType, ParsedValue)]
           -> EnvironmentProxy env
           -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> TC (Code env'))
           -> TC (Code env)
spliceArgs _  []                       env k = withEnvironment env (k env)
spliceArgs sr ((fresh, ann, arg) : rest) env k = withEnvironment env $ do
  SomeType (pty :: TypeProxy pty) <- inferArgType sr ann arg env
  withKnownType pty $ do
    argVal <- atType arg pty :: TC (Value '(env, pty))
    letBind sr fresh pty argVal env $ \env' -> spliceArgs sr rest env' k

-- | Bind a (fresh) name to a value with a @Let@, extending the environment and
-- wrapping the continuation's code in that @Let@.
letBind :: forall env ty
         . SourceRange -> String -> TypeProxy ty -> Value '(env, ty) -> EnvironmentProxy env
        -> (forall name. (KnownSymbol name, NotPresent name env)
              => EnvironmentProxy ('(name, ty) ': env) -> TC (Code ('(name, ty) ': env)))
        -> TC (Code env)
letBind sr nm ty v env k = withEnvironment env $ case someSymbolVal nm of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' pf -> recallIsAbsent pf $
      Let bindingEvidence name v <$> k (bindNameEnv name ty pf env)
    Found' _ _ -> throwError (Internal (AlreadyDefined sr nm))

-- | Infer (or check, if annotated) the type of an argument in the given
-- environment. The explicit annotations pin the environment.
inferArgType :: forall env
              . SourceRange -> Maybe SomeType -> ParsedValue -> EnvironmentProxy env -> TC SomeType
inferArgType sr ann arg env = withEnvironment env $ case ann of
  Just (SomeType (pty :: TypeProxy pty)) ->
    withKnownType pty ((atType arg pty :: TC (Value '(env, pty))) $> SomeType pty)
  Nothing -> tryEachType (Advice sr ("I couldn't infer the type of an argument."))
    [ (atType arg IntegerType :: TC (Value '(env, 'IntegerT))) $> SomeType IntegerType
    , (atType arg RealType    :: TC (Value '(env, 'RealT)))    $> SomeType RealType
    , (atType arg ComplexType :: TC (Value '(env, 'ComplexT))) $> SomeType ComplexType
    , (atType arg BooleanType :: TC (Value '(env, 'BooleanT))) $> SomeType BooleanType
    , (atType arg ColorType   :: TC (Value '(env, 'ColorT)))   $> SomeType ColorType
    ]

-- | A default value used to initialise a compound function's result slot
-- before its body runs.
defaultFor :: forall env ty. KnownEnvironment env => SourceRange -> TypeProxy ty -> TC (Value '(env, ty))
defaultFor sr = \case
  IntegerType -> pure (Const (Scalar IntegerType 0))
  RealType    -> pure (Const (Scalar RealType 0))
  ComplexType -> pure (Const (Scalar ComplexType 0))
  BooleanType -> pure (Const (Scalar BooleanType False))
  ColorType   -> pure (Const (Scalar ColorType black))
  t           -> throwError (Advice sr ("Functions returning " ++ showType t
                   ++ " can't be used this way yet."))
