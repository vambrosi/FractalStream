{-# language AllowAmbiguousTypes #-}

-- | Forward-mode differentiation of code with loops (dual numbers).
--
-- * Each tracked variable has a /shadow/ (a runtime variable holding its
--   derivative, updated alongside it).
-- * 'dValue' differentiates a 'Value'; 'dualizeCode' a 'Code'.
--
-- Every name a pass introduces (shadows and shared temporaries) is salted
-- with that pass's id, so the transform can be applied to its own output
-- (a second derivative) without name collisions. 'Tracked' maps each
-- variable to its shadow.
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
  , InternalIterations, InternalStuck, InternalSolution
  )
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

-- | Tracked variable name -> shadow name (declared and in scope).
type Tracked = Map String String

-- | The shadow name of @name@ in pass @gen@ (an id unique to the pass).
-- Unique because @name@ is unique within a 'Code'.
--
-- Not 'Language.Code.Typecheck.withFresh' (it only avoids names in the
-- given environment, not those inside an earlier pass's output).
freshShadowName :: String -> String -> String
freshShadowName gen name = "[dual " ++ gen ++ " of " ++ name ++ "]"

-- | Differentiate @v@ (tracked variables read their shadow, everything else
-- is constant). @blame@ only fills in the error message.
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

-- | Real or Complex. Other types (loop counters, flags) are never tracked.
isDifferentiable :: TypeProxy ty -> Bool
isDifferentiable = \case
  ComplexType -> True
  RealType    -> True
  _           -> False

-- ---------------------------------------------------------------------------
-- Sharing
--
-- Some derivative rules repeat an operand, e.g. the quotient rule uses the
-- denominator three times. Before differentiating, the operands of @/@,
-- @|.|@ and @^@ are moved into their own tracked @Let@s, so each is computed
-- once. Otherwise a second derivative can duplicate them many times.
--
-- * 'extractIfNontrivial' moves one operand into a @Let@ with a shadow.
-- * 'liftSharedGeneric' walks a value and extracts at each such node.
extractIfNontrivial
  :: forall env ty
   . SourceRange -> String -> String
  -> EnvironmentProxy env -> Tracked -> TypeProxy ty -> Value '(env, ty)
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> Value '(env', ty) -> TC (Code env'))
  -> TC (Code env)
extractIfNontrivial sr blame gen env tracked ty v k = case v of
  Var{}   -> k env tracked v
  Const{} -> k env tracked v
  _ -> withEnvironment env $ do
    let tmpName = "[internal] shared " ++ gen ++ " #" ++ show (length $ fromEnvironment env (\_ _ -> ()))
    dv <- dValue blame tracked sr v
    letBind sr tmpName ty v env $ \env1 ->
      letBind sr (freshShadowName gen tmpName) ty (reindexValue env1 dv) env1 $ \env2 ->
        case someSymbolVal tmpName of
          SomeSymbol name -> do
            pf <- findVarAtType sr name ty env2
            withKnownType ty $
              k env2 (Map.insert tmpName (freshShadowName gen tmpName) tracked) (Var name ty pf)

extractIfNontrivialWirtinger
  :: forall env ty
   . SourceRange -> String -> String
  -> EnvironmentProxy env -> Tracked -> TypeProxy ty -> Value '(env, ty)
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> Value '(env', ty) -> TC (Code env'))
  -> TC (Code env)
extractIfNontrivialWirtinger sr blame gen env tracked ty v k = case v of
  Var{}   -> k env tracked v
  Const{} -> k env tracked v
  _ -> withEnvironment env $ do
    let tmpName = "[internal] shared " ++ gen ++ " #" ++ show (length $ fromEnvironment env (\_ _ -> ()))
    dv <- dValueWirtinger blame tracked sr v
    letBind sr tmpName ty v env $ \env1 ->
      letBind sr (freshShadowName gen tmpName) ComplexType (reindexValue env1 dv) env1 $ \env2 ->
        case someSymbolVal tmpName of
          SomeSymbol name -> do
            pf <- findVarAtType sr name ty env2
            withKnownType ty $
              k env2 (Map.insert tmpName (freshShadowName gen tmpName) tracked) (Var name ty pf)

liftSharedGeneric
  :: forall env ty
   . SourceRange -> String -> String
  -> (forall e t. EnvironmentProxy e -> Tracked -> TypeProxy t -> Value '(e, t)
      -> (forall e'. KnownEnvironment e' => EnvironmentProxy e' -> Tracked -> Value '(e', t) -> TC (Code e'))
      -> TC (Code e))
  -> EnvironmentProxy env -> Tracked -> TypeProxy ty -> Value '(env, ty)
  -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> Value '(env', ty) -> TC (Code env'))
  -> TC (Code env)
liftSharedGeneric sr blame gen extract env tracked _ty v0 k = case v0 of

  -- The three "sharing" operators.
  DivF x y -> go env tracked RealType x $ \envX trackedX x' ->
    go envX trackedX RealType (reindexValue envX y) $ \envY trackedY y' ->
      extract envY trackedY RealType y' $ \envY' trackedY' y'' ->
        k envY' trackedY' (DivF (reindexValue envY' x') y'')

  DivC x y -> go env tracked ComplexType x $ \envX trackedX x' ->
    go envX trackedX ComplexType (reindexValue envX y) $ \envY trackedY y' ->
      extract envY trackedY ComplexType y' $ \envY' trackedY' y'' ->
        k envY' trackedY' (DivC (reindexValue envY' x') y'')

  PowF x n -> go env tracked RealType x $ \envX trackedX x' ->
    go envX trackedX RealType (reindexValue envX n) $ \envN trackedN n' ->
      extract envN trackedN RealType n' $ \envN' trackedN' n'' ->
        k envN' trackedN' (PowF (reindexValue envN' x') n'')

  PowC x n -> go env tracked ComplexType x $ \envX trackedX x' ->
    go envX trackedX ComplexType (reindexValue envX n) $ \envN trackedN n' ->
      extract envN trackedN ComplexType n' $ \envN' trackedN' n'' ->
        k envN' trackedN' (PowC (reindexValue envN' x') n'')

  AbsF x -> go env tracked RealType x $ \envX trackedX x' ->
    extract envX trackedX RealType x' $ \envX' trackedX' x'' ->
      k envX' trackedX' (AbsF x'')

  AbsC x -> go env tracked ComplexType x $ \envX trackedX x' ->
    extract envX trackedX ComplexType x' $ \envX' trackedX' x'' ->
      k envX' trackedX' (AbsC x'')

  -- Everything else (recurse into the children).
  AddF x y -> rec2 RealType RealType x y AddF
  SubF x y -> rec2 RealType RealType x y SubF
  MulF x y -> rec2 RealType RealType x y MulF
  ModF x y -> rec2 RealType RealType x y ModF
  Arctan2F x y -> rec2 RealType RealType x y Arctan2F

  AddC x y -> rec2 ComplexType ComplexType x y AddC
  SubC x y -> rec2 ComplexType ComplexType x y SubC
  MulC x y -> rec2 ComplexType ComplexType x y MulC

  AddI x y -> rec2 IntegerType IntegerType x y AddI
  SubI x y -> rec2 IntegerType IntegerType x y SubI
  MulI x y -> rec2 IntegerType IntegerType x y MulI
  DivI x y -> rec2 IntegerType IntegerType x y DivI
  ModI x y -> rec2 IntegerType IntegerType x y ModI
  PowI x y -> rec2 IntegerType IntegerType x y PowI

  Or  x y -> rec2 BooleanType BooleanType x y Or
  And x y -> rec2 BooleanType BooleanType x y And

  Eql t x y -> rec2 t t x y (Eql t)
  NEq t x y -> rec2 t t x y (NEq t)
  LTI x y -> rec2 IntegerType IntegerType x y LTI
  LTF x y -> rec2 RealType RealType x y LTF

  NegF x -> rec1 RealType x NegF
  RoundF x -> rec1 RealType x RoundF
  FloorF x -> rec1 RealType x FloorF
  CeilingF x -> rec1 RealType x CeilingF
  ExpF x -> rec1 RealType x ExpF
  LogF x -> rec1 RealType x LogF
  SqrtF x -> rec1 RealType x SqrtF
  SinF x -> rec1 RealType x SinF
  CosF x -> rec1 RealType x CosF
  TanF x -> rec1 RealType x TanF
  SinhF x -> rec1 RealType x SinhF
  CoshF x -> rec1 RealType x CoshF
  TanhF x -> rec1 RealType x TanhF
  ArcsinF x -> rec1 RealType x ArcsinF
  ArccosF x -> rec1 RealType x ArccosF
  ArctanF x -> rec1 RealType x ArctanF
  ArcsinhF x -> rec1 RealType x ArcsinhF
  ArccoshF x -> rec1 RealType x ArccoshF
  ArctanhF x -> rec1 RealType x ArctanhF

  NegC x -> rec1 ComplexType x NegC
  ArgC x -> rec1 ComplexType x ArgC
  ReC x -> rec1 ComplexType x ReC
  ImC x -> rec1 ComplexType x ImC
  ConjC x -> rec1 ComplexType x ConjC
  ExpC x -> rec1 ComplexType x ExpC
  LogC x -> rec1 ComplexType x LogC
  SqrtC x -> rec1 ComplexType x SqrtC
  SinC x -> rec1 ComplexType x SinC
  CosC x -> rec1 ComplexType x CosC
  TanC x -> rec1 ComplexType x TanC
  SinhC x -> rec1 ComplexType x SinhC
  CoshC x -> rec1 ComplexType x CoshC
  TanhC x -> rec1 ComplexType x TanhC

  AbsI x -> rec1 IntegerType x AbsI
  NegI x -> rec1 IntegerType x NegI
  Not x -> rec1 BooleanType x Not

  I2R x -> rec1 IntegerType x I2R
  R2C x -> rec1 RealType x R2C

  ITE t c yes no ->
    go env tracked BooleanType c $ \envC trackedC c' ->
      go envC trackedC t (reindexValue envC yes) $ \envY trackedY yes' ->
        go envY trackedY t (reindexValue envY no) $ \envN trackedN no' ->
          k envN trackedN (ITE t (reindexValue envN c') (reindexValue envN yes') no')

  -- Anything else is left alone. Some of these (e.g. LocalLet) bind
  -- names, so lifting through them would need care.
  _ -> withEnvironment env $ k env tracked v0

 where
  -- Indented less than the `case` alternatives, so it closes the `case`.
  go :: forall e t. EnvironmentProxy e -> Tracked -> TypeProxy t -> Value '(e, t)
     -> (forall e'. KnownEnvironment e' => EnvironmentProxy e' -> Tracked -> Value '(e', t) -> TC (Code e'))
     -> TC (Code e)
  go = liftSharedGeneric sr blame gen extract

  rec1 :: forall cty
        . TypeProxy cty -> Value '(env, cty)
       -> (forall e. KnownEnvironment e => Value '(e, cty) -> Value '(e, ty))
       -> TC (Code env)
  rec1 cty x rebuild = go env tracked cty x $ \env' tracked' x' -> k env' tracked' (rebuild x')

  rec2 :: forall cty1 cty2
        . TypeProxy cty1 -> TypeProxy cty2 -> Value '(env, cty1) -> Value '(env, cty2)
       -> (forall e. KnownEnvironment e => Value '(e, cty1) -> Value '(e, cty2) -> Value '(e, ty))
       -> TC (Code env)
  rec2 cty1 cty2 x y rebuild =
    go env tracked cty1 x $ \envX trackedX x' ->
      go envX trackedX cty2 (reindexValue envX y) $ \envY trackedY y' ->
        k envY trackedY (rebuild (reindexValue envY x') y')

-- | 'liftSharedGeneric' with same-type shadows.
liftShared :: forall env ty
            . SourceRange -> String -> String
           -> EnvironmentProxy env -> Tracked -> TypeProxy ty -> Value '(env, ty)
           -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> Value '(env', ty) -> TC (Code env'))
           -> TC (Code env)
liftShared sr blame gen = liftSharedGeneric sr blame gen (extractIfNontrivial sr blame gen)

-- | 'liftSharedGeneric' with complex shadows.
liftSharedWirtinger :: forall env ty
                      . SourceRange -> String -> String
                     -> EnvironmentProxy env -> Tracked -> TypeProxy ty -> Value '(env, ty)
                     -> (forall env'. KnownEnvironment env' => EnvironmentProxy env' -> Tracked -> Value '(env', ty) -> TC (Code env'))
                     -> TC (Code env)
liftSharedWirtinger sr blame gen = liftSharedGeneric sr blame gen (extractIfNontrivialWirtinger sr blame gen)

-- | Add shadow updates to a 'Code', so every tracked variable's shadow holds
-- its running derivative.
--
-- * Variables in @tracked@ must already have declared shadows.
-- * Real/Complex @Let@s in the code get new shadows, named with @gen@.
-- * Lists and draw commands are unsupported.
-- * @blame@ only fills in the error message.
dualizeCode :: SourceRange -> String -> String -> Tracked -> Code env -> TC (Code env)
dualizeCode sr blame gen tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        liftShared sr blame gen env tracked ty v $ \env0 tracked0 v' -> do
          dv <- dValue blame tracked0 sr v'
          letBind sr (symbolVal name) ty v' env0 $ \env1 ->
            letBind sr (freshShadowName gen (symbolVal name)) ty (reindexValue env1 dv) env1 $ \env2 ->
              dualizeCode sr blame gen
                (Map.insert (symbolVal name) (freshShadowName gen (symbolVal name)) tracked0)
                (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCode sr blame gen tracked body

  Set _pf name v -> case Map.lookup (symbolVal name) tracked of
    Just shadowNm | isDifferentiable (typeOfValue v) -> do
      let ty = typeOfValue v
          env = envProxy Proxy
      liftShared sr blame gen env tracked ty v $ \env' tracked' v' -> withEnvironment env' $ do
        dv <- dValue blame tracked' sr v'
        pf' <- findVarAtType sr name ty env'
        case someSymbolVal shadowNm of
          SomeSymbol sname -> case lookupEnv sname ty env' of
            -- Update the shadow first. (`dv` reads the old value of `name`,
            -- e.g. in `w <- w * x`.)
            Found spf -> pure (Block [ Set spf sname dv, Set pf' name v' ])
            _ -> throwError (Advice sr
                   ("dualizeCode: internal error, `" ++ symbolVal name
                     ++ "` is tracked but its shadow is missing."))
    _ -> pure (Set _pf name v)

  Block stmts -> Block <$> traverse (dualizeCode sr blame gen tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCode sr blame gen tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCode sr blame gen tracked yes <*> dualizeCode sr blame gen tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCode: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCode: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCode: list operations are not supported.")

-- | Wirtinger version of 'dValue' (every shadow and result is complex,
-- whatever the variable's type). Needed when a complex seed flows into real
-- values, e.g. @log(|x|)@.
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

-- | Wirtinger version of 'dualizeCode' (all shadows are complex).
dualizeCodeWirtinger :: SourceRange -> String -> String -> Tracked -> Code env -> TC (Code env)
dualizeCodeWirtinger sr blame gen tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        liftSharedWirtinger sr blame gen env tracked ty v $ \env0 tracked0 v' -> do
          dv <- dValueWirtinger blame tracked0 sr v'
          letBind sr (symbolVal name) ty v' env0 $ \env1 ->
            letBind sr (freshShadowName gen (symbolVal name)) ComplexType (reindexValue env1 dv) env1 $ \env2 ->
              dualizeCodeWirtinger sr blame gen
                (Map.insert (symbolVal name) (freshShadowName gen (symbolVal name)) tracked0)
                (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCodeWirtinger sr blame gen tracked body

  Set _pf name v -> case Map.lookup (symbolVal name) tracked of
    Just shadowNm | isDifferentiable (typeOfValue v) -> do
      let ty = typeOfValue v
          env = envProxy Proxy
      liftSharedWirtinger sr blame gen env tracked ty v $ \env' tracked' v' -> withEnvironment env' $ do
        dv <- dValueWirtinger blame tracked' sr v'
        pf' <- findVarAtType sr name ty env'
        case someSymbolVal shadowNm of
          SomeSymbol sname -> case lookupEnv sname ComplexType env' of
            Found spf -> pure (Block [ Set spf sname dv, Set pf' name v' ])
            _ -> throwError (Advice sr
                   ("dualizeCodeWirtinger: internal error, `" ++ symbolVal name
                     ++ "` is tracked but its shadow is missing."))
    _ -> pure (Set _pf name v)

  Block stmts -> Block <$> traverse (dualizeCodeWirtinger sr blame gen tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCodeWirtinger sr blame gen tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCodeWirtinger sr blame gen tracked yes <*> dualizeCodeWirtinger sr blame gen tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCodeWirtinger: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCodeWirtinger: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCodeWirtinger: list operations are not supported.")

-- | Bind each argument to its fresh parameter name, and each differentiable
-- one's derivative to a shadow. Arguments are all differentiated with
-- respect to @tracked0@, since they can't refer to each other.
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

-- | Splice a compound call, differentiated with respect to @tracked0@.
-- The result @F@ and its derivative @F'@ are local variables (the body may
-- loop), passed to the continuation.
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

-- | Wirtinger version of 'spliceArgsDual' (all shadows are complex).
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

-- | Wirtinger version of 'spliceCompoundDual' (@F@ has type @ty@, @F'@ is
-- always complex).
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

-- | @solve z -> f(args)@ for a compound @f@, differentiated with
-- 'dualizeCode'.
--
-- @F@ and @F'@ are computed by running the spliced body, so the splice
-- appears twice (before the loop, and after each step for the next
-- convergence check).
tcSolveCompound :: String
                -> (CompoundFunction, [ParsedValue])
                -> Maybe ParsedValue
                -> Maybe ParsedValue
                -> CheckedCode
tcSolveCompound var (cf, args) mtol mlimit sr (env :: EnvironmentProxy env) = do

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
                  -- `solution` is the root z, not F(z).
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
                  -- `solution` is the root z, not F(z).
                  , Set solpf (Proxy @InternalSolution)
                      (withEnvironment envZ $ ITE ComplexType stuckCond nanSolution (R2C (Var zname RealType zpf')))
                  , Set zpf'  zname (Var saveName RealType savePf')
                  ]

          _ -> throwError (Advice sr ("`solve` needs a real or complex unknown, but `"
                 ++ var ++ "` is " ++ showType zty ++ "."))

-- | @critical z -> f(args)@ for a compound @f@ (Newton on @g = F'@ with
-- @g' = F''@).
--
-- * One splice gives @(F, g)@.
-- * Running 'dualizeCode' over that whole splice, with a second shadow of
--   @z@, gives @g'@ as the shadow of @g@.
--
-- As in 'tcSolveCompound', the splice appears before the loop and after
-- each step.
tcCriticalCompound :: String
                   -> (CompoundFunction, [ParsedValue])
                   -> Maybe ParsedValue
                   -> Maybe ParsedValue
                   -> CheckedCode
tcCriticalCompound var (cf, args) mtol mlimit sr (env :: EnvironmentProxy env) = do

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

                   -- The first pass puts (F, g) into fName/dfName. The second
                   -- pass, over the result and tracking dfName -> ddfName,
                   -- puts g' into ddfName.
                   -- Both seeds are declared before `fos` is built
                   -- (`withFresh` can't see names inside `fos`).
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
                 -- `solution` is the critical point z, not g(z).
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

                   -- Both seeds are declared before `fos` is built.
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
                 -- `solution` is the critical point z, not g(z).
                 , Set solpf (Proxy @InternalSolution)
                     (withEnvironment envFDD $ ITE ComplexType stuckCond nanSolution (R2C (Var zname RealType zpf')))
                 , Set zpf'  zname (Var saveName RealType savePf')
                 ]

          _ -> throwError (Advice sr ("`critical` needs a real or complex unknown, but `"
                 ++ var ++ "` is " ++ showType zty ++ "."))
