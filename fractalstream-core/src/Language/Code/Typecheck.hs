module Language.Code.Typecheck where

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Typecheck
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Parser.SourceRange

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

tcWhile :: ParsedValue -> ParsedCode -> CheckedCode
tcWhile cond body _sr env =
  IfThenElse <$> atType cond BooleanType
             <*> (DoWhile <$> atType cond BooleanType <*> atEnv env body)
             <*> pure NoOp

tcDoWhile :: ParsedCode -> ParsedValue -> CheckedCode
tcDoWhile body cond _sr env =
  DoWhile <$> atType cond BooleanType <*> atEnv env body

tcUntil :: ParsedValue -> ParsedCode -> CheckedCode
tcUntil cond body _sr env =
  IfThenElse <$> atType cond BooleanType
             <*> (DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body)
             <*> pure NoOp

tcDoUntil :: ParsedCode -> ParsedValue -> CheckedCode
tcDoUntil body cond _sr env =
  DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body

tcIfThenElse :: ParsedValue -> ParsedCode -> ParsedCode -> CheckedCode
tcIfThenElse cond yes no _sr env =
  IfThenElse <$> atType cond BooleanType
             <*> atEnv env yes
             <*> atEnv env no

tcPoint :: KnownEnvironment env => ParsedValue -> TC (Value '(env, 'Pair 'RealT 'RealT))
tcPoint p@(ParsedValue sr _) =
  tryEach (Surprise sr "this"
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

tcListInsert :: ParsedValue -> StartOrEnd -> String -> CheckedCode
tcListInsert item soe listName sr env = do
  SomeSymbol list <- pure (someSymbolVal listName)
  ListExists itemTy pfListPresent <- getListType sr list env
  Insert pfListPresent list (ListType itemTy) env soe <$> atType item itemTy

tcListRemoveSome :: String -> ParsedValue -> String -> CheckedCode
tcListRemoveSome itemName predicate listName sr env = do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  ListExists itemTy pfListPresent <- getListType sr list env
  DeclaredVar pfItemAbsent _ <- declareVar sr item itemTy env
  Remove pfListPresent list (ListType itemTy) item pfItemAbsent env
    <$> atType predicate BooleanType

tcListRemoveAll :: String -> CheckedCode
tcListRemoveAll listName sr env = do
  SomeSymbol list <- pure (someSymbolVal listName)
  ListExists itemTy pfListPresent <- getListType sr list env
  pure (ClearList pfListPresent list (ListType itemTy) env)

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
