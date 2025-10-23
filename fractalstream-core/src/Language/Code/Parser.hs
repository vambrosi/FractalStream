{-# language RecursiveDo, DataKinds #-}
module Language.Code.Parser
  ( parseCode
  , Errs(..)
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser hiding (many)
import Language.Value
import Language.Value.Parser hiding (parseValueFromTokens)
import Language.Code
import Language.Draw

import qualified Data.Set as Set

newtype EnvCode = EnvCode
  (forall env. EnvironmentProxy env -> Errs (Code env))

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either String (Code env)
parseCode env splices input = do
  EnvCode c <- parseEnvCode splices input
  case c env of
    Errs (Left errs) -> Left (init . unlines . Set.toList . Set.fromList $ errs)
    Errs (Right c')  -> pure c'

parseEnvCode :: Splices
             -> String
             -> Either String EnvCode
parseEnvCode splices input = do
  let toks = tokenizeWithIndentation input
  case fullParses (parser (codeGrammar splices)) toks of
    ([], r)   -> Left ("no parse: " ++ show r)
    ([c], _)  -> pure c
    (_, r)    -> Left ("ambiguous parse: " ++ show r)

ident :: ProdSR r String String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing

tok :: String -> ProdSR r String ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  _ -> Nothing

codeGrammar :: forall r
             . Splices
            -> Grammar r (ProdSR r String EnvCode)
codeGrammar splices = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  block <- ruleChoice
    [ withSourceRange (mkBlock <$> (token Indent *> many toplevel <* token Dedent)) <?> "indented statements"
    -- Grammar hack so that Let statements slurp up the remainder
    -- of the block into their scope.
    , withSourceRange (
        ((\xs x -> mkBlock (xs ++ [x]))
         <$> (token Indent *> many toplevel)
         <*> (letStatement <* token Dedent)) <?> "indented statements")
    ]

  letStatement <- rule $
    withSourceRange (
     mkLet <$> ident
           <*> (token Colon *> typ)
           <*> (token LeftArrow *> value <* token Newline)
           <*> (withSourceRange (mkBlock <$> blockTail)) <?> "variable initialization")

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* token Newline
    , withSourceRange
      (mkIfThenElse <$> (token If *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , withSourceRange
      (mkWhile <$> (lit "while" *> value <* token Newline)
               <*> block) <?> "while loop"
    , withSourceRange
      (mkDoWhile <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "while" *> value <* token Newline))
    , withSourceRange
      (mkUntil <$> (lit "until" *> value <* token Newline)
               <*> block) <?> "until loop"
    , withSourceRange
      (mkDoUntil <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "until" *> value <* token Newline))
    , effect <?> "extended operation"
    ]

  elseIf <- ruleChoice
    [ ((token Else *> token Newline) *> block) <?> "else clause"
    , withSourceRange
      (mkIfThenElse <$> ((token Else *> token If) *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "else if clause"
    , withSourceRange (pure noOp) <?> "end of block"
    ]

  let lit = token . Identifier
      noOp = \_sr -> EnvCode $ \env -> withEnvironment env $ pure NoOp

  simpleStatement <- ruleChoice
    [ withSourceRange
      (mkSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , withSourceRange (noOp <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammar splices

  effect <- ruleChoice
    [ toplevelDrawCommand
    , listCommand
    ]

  toplevelDrawCommand <- ruleChoice
    [ (tok "draw" *> drawCommand <* token Newline)
    , (tok "use" *> penCommand <* token Newline)
    , (tok "erase" *> eraseCommand <* token Newline)
    ]

  eraseCommand <- ruleChoice [withSourceRange (pure mkClear)]

  drawCommand <- ruleChoice
    [ withSourceRange
      (mkDrawPoint <$> ((tok "point" *> tok "at") *> value))
    , withSourceRange
      (mkDrawCircle <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "circle" *> tok "at") *> value)
                    <*> ((tok "with" *> tok "radius") *> value))
    , withSourceRange
      (mkDrawRect <$> (isJust <$> optional (tok "filled"))
                  <*> ((tok "rectangle" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    , withSourceRange
      (mkDrawLine <$> ((tok "line" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ withSourceRange (mkSetFill   <$> (value <* (tok "for" *> tok "fill")))
    , withSourceRange (mkSetStroke <$> (value <* (tok "for" *> strokeOrLine)))
    ]

  startOrEnd <- ruleChoice
    [ tok "start" $> Start
    , tok "end" $> End ]

  listCommand <- ruleChoice
    [ withSourceRange (mkListInsert
      <$> (tok "insert" *> value <* tok "at")
      <*> (startOrEnd <* tok "of")
      <*> (ident <* token Newline))
    , withSourceRange (mkListRemoveSome
      <$> (tok "remove" *> tok "each" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "from" *> ident <* token Newline))
    , withSourceRange (mkListRemoveAll
      <$> (tok "remove" *> tok "all" *> tok "items" *> tok "from" *> ident <* token Newline))
    , withSourceRange (mkListFor
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block))
    , withSourceRange (mkListWith
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block)
      <*> optional (tok "else" *> token Newline *> block))
    ]
  pure toplevel

atEnv :: EnvironmentProxy env -> EnvCode -> Errs (Code env)
atEnv env (EnvCode f) = f env

mkBlock :: [EnvCode] -> SourceRange -> EnvCode
mkBlock body _ = EnvCode $ \env ->
  withEnvironment env $ do
    Block <$> traverse (atEnv env) body

mkLet :: String -> FSType -> TypedValue -> EnvCode -> SourceRange -> EnvCode
mkLet n t v c sr = EnvCode $ \env -> withType t $ \ty ->
  withEnvironment env $ do
    -- Get a proof that `name` is not already bound in the environment `env`.
    -- Make an extended environment env' that binds name to ty
    -- Evaluate c in this extended environment.
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv name ty env of
      Found _ -> Errs $ Left [n ++ " is already bound in this environment, loc=" ++ show sr]
      WrongType _ -> Errs $ Left [n ++ " is already bound in this environment, loc=" ++ show sr]
      Absent absent -> recallIsAbsent absent $ do
        let pf = bindName name ty absent
        val <- atType v ty
        let env' = BindingProxy name ty env
        Let pf name val <$> atEnv env' c

mkSet :: String -> TypedValue -> SourceRange -> EnvCode
mkSet n v sr = EnvCode $ \env ->
  withEnvironment env $ do
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv' name env of
      Found' ty pf -> Set pf name <$> atType v ty
      Absent' _ -> Errs $ Left [n ++ " is not in scope here. loc=" ++ show sr]

mkWhile :: TypedValue -> EnvCode -> SourceRange -> EnvCode
mkWhile cond body _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> atType cond BooleanType <*> atEnv env body)
               <*> pure NoOp

mkDoWhile :: EnvCode -> TypedValue -> SourceRange -> EnvCode
mkDoWhile body cond _sr = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> atType cond BooleanType <*> atEnv env body

mkUntil :: TypedValue -> EnvCode -> SourceRange -> EnvCode
mkUntil cond body _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body)
               <*> pure NoOp

mkDoUntil :: EnvCode -> TypedValue -> SourceRange -> EnvCode
mkDoUntil body cond _sr = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body

mkIfThenElse :: TypedValue -> EnvCode -> EnvCode -> SourceRange -> EnvCode
mkIfThenElse cond yes no _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> atEnv env yes
               <*> atEnv env no


mkDrawPoint :: TypedValue
            -> SourceRange
            -> EnvCode
mkDrawPoint v _sr = EnvCode $ \env -> withEnvironment env $
  (DrawCommand . DrawPoint env <$> atType v (PairType RealType RealType)) <|>
  (DrawCommand . DrawPoint env . C2R2 <$> atType v ComplexType)

mkDrawCircle :: Bool
             -> TypedValue
             -> TypedValue
             -> SourceRange
             -> EnvCode
mkDrawCircle isFilled center radius _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawCircle env isFilled
    <$> atType radius RealType
    <*> (atType center (PairType RealType RealType) <|>
         (C2R2 <$> atType center ComplexType)))

mkDrawRect :: Bool
           -> TypedValue
           -> TypedValue
           -> SourceRange
           -> EnvCode
mkDrawRect isFilled ul lr _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawRect env isFilled
    <$> (atType ul (PairType RealType RealType) <|>
         (C2R2 <$> atType ul ComplexType))
    <*> (atType lr (PairType RealType RealType) <|>
         (C2R2 <$> atType lr ComplexType)))

mkDrawLine :: TypedValue
           -> TypedValue
           -> SourceRange
           -> EnvCode
mkDrawLine ul lr _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawLine env
    <$> (atType ul (PairType RealType RealType) <|>
         (C2R2 <$> atType ul ComplexType))
    <*> (atType lr (PairType RealType RealType) <|>
         (C2R2 <$> atType lr ComplexType)))

mkSetStroke :: TypedValue -> SourceRange -> EnvCode
mkSetStroke c _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand . SetStroke env <$> atType c ColorType

mkSetFill :: TypedValue -> SourceRange -> EnvCode
mkSetFill c _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand . SetFill env <$> atType c ColorType

mkClear :: SourceRange -> EnvCode
mkClear _sr = EnvCode (\env -> withEnvironment env $ pure (DrawCommand $ Clear env))

mkListInsert :: TypedValue -> StartOrEnd -> String -> SourceRange -> EnvCode
mkListInsert item soe listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> Errs $ Left [listName ++ " is not defined in this environment, loc=" ++ show sr]
    Found' listTy pfListPresent -> case listTy of
      ListType itemTy -> do
        Insert pfListPresent list listTy env soe <$> atType item itemTy
      ty -> Errs $ Left [listName ++ " is not a list, its type is " ++ showType ty ++ ", loc=" ++ show sr]

mkListRemoveSome :: String -> TypedValue -> String -> SourceRange -> EnvCode
mkListRemoveSome itemName predicate listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> Errs $ Left [listName ++ " is not defined in this environment, loc=" ++ show sr]
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {} -> Errs $ Left [itemName ++ " is already defined in this environment, loc=" ++ show sr]
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ case listTy of
          ListType _ -> do
            Remove pfListPresent list listTy item pfItemAbsent env
              <$> atType predicate BooleanType
          ty -> Errs $ Left [listName ++ " is not a list, its type is " ++ showType ty ++ ", loc=" ++ show sr]

mkListRemoveAll :: String -> SourceRange -> EnvCode
mkListRemoveAll listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> Errs $ Left [listName ++ " is not defined in this environment, loc=" ++ show sr]
    Found' listTy pfListPresent -> case listTy of
      ListType _ -> pure (ClearList pfListPresent list listTy env)
      ty -> Errs $ Left [listName ++ " is not a list, its type is " ++ showType ty]

mkListFor :: String -> String -> EnvCode -> SourceRange -> EnvCode
mkListFor itemName listName body sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> Errs $ Left [listName ++ " is not defined in this environment, loc=" ++ show sr]
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {} -> Errs $ Left [itemName ++ " is already defined in this environment, loc=" ++ show sr]
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ do
          case listTy of
            ListType itemTy -> do
              let env' = declare itemTy env
              ForEach pfListPresent list listTy item pfItemAbsent env env' <$> atEnv env' body
            ty -> Errs $ Left [listName ++ " is not a list, its type is " ++ showType ty ++ ", loc=" ++ show sr]

mkListWith :: String
           -> TypedValue
           -> String
           -> EnvCode
           -> Maybe EnvCode
           -> SourceRange
           -> EnvCode
mkListWith itemName predicate listName body fallback sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> Errs $ Left [listName ++ " is not defined in this environment, loc=" ++ show sr]
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {}  -> Errs $ Left [itemName ++ " is already defined in this environment, loc=" ++ show sr]
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ do
          case listTy of
            ListType itemTy -> do
              let env' = declare itemTy env
              Lookup pfListPresent list listTy item pfItemAbsent env' env
                <$> atType predicate BooleanType
                <*> atEnv env' body
                <*> traverse (atEnv env) fallback
            ty -> Errs $ Left [listName ++ " is not a list, its type is " ++ showType ty ++ ", loc=" ++ show sr]
