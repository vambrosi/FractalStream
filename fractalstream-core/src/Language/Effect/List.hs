{-# language OverloadedStrings, RecursiveDo #-}

module Language.Effect.List
  ( List(..)
  , StartOrEnd(..)
  , listEffectParser
  ) where

import FractalStream.Prelude

import Language.Value
import Data.Indexed.Functor
import Language.Code.Parser
import Language.Value.Parser
import Language.Parser
import Language.Effect

data StartOrEnd = Start | End

data List (listName :: Symbol) (listType :: FSType) (code :: Environment -> Exp Type) (env :: Environment) where

  Insert :: forall name ty env code
          . Proxy name
         -> TypeProxy ty
         -> StartOrEnd
         -> EnvironmentProxy env
         -> Value '(env, ty)
         -> List name ty code env

  Lookup :: forall name ty env code item
          . KnownSymbol item
         => Proxy name
         -> TypeProxy ty
         -> Proxy item
         -> NameIsAbsent item env
         -> EnvironmentProxy ('(item, ty) ': env)
         -> EnvironmentProxy env
         -> Value '( '(item, ty) ': env, 'BooleanT)
         -- ^ predicate to test for
         -> Eval (code ( '(item, ty) ': env))
         -- ^ action to run on the first item matching the predicate
         -> Maybe (Eval (code env))
         -- ^ optional fallback action to run if there was no match
         -> List name ty code env

  ClearList :: forall name ty env code
             . Proxy name
            -> EnvironmentProxy env
            -> List name ty code env

  Remove :: forall name ty env code item
          . KnownSymbol item
         => Proxy name
         -> TypeProxy ty
         -> Proxy item
         -> NameIsAbsent item env
         -> EnvironmentProxy env
         -> Value '( '(item, ty) ': env, 'BooleanT)
         -> List name ty code env

  ForEach :: forall name ty env code item
           . KnownSymbol item
          => Proxy name
          -> TypeProxy ty
          -> Proxy item
          -> NameIsAbsent item env
          -> EnvironmentProxy env
          -> EnvironmentProxy ('(item, ty) ': env)
          -> Eval (code ( '( item, ty) ': env))
          -> List name ty code env

instance IFunctor (List name ty) where
  type IndexProxy (List name ty) = EnvironmentProxy

  imap :: forall a b i
        . (forall j. EnvironmentProxy j -> Eval (a j) -> Eval (b j))
       -> List name ty a i
       -> List name ty b i
  imap f = \case
    Insert name ty soe env v -> Insert name ty soe env v
    Lookup name ty item pf env env' test match miss ->
      Lookup name ty item pf env env' test (f env match) (f env' <$> miss)
    ClearList name env -> ClearList name env
    Remove name ty item pf env v -> Remove name ty item pf env v
    ForEach name ty item pf env env' body -> ForEach name ty item pf env env' (f env' body)

  toIndex = \case
    Insert _ _ _ env _         -> env
    Lookup _ _ _ _ _ env _ _ _ -> env
    ClearList _ env            -> env
    Remove _ _ _ _ env _       -> env
    ForEach _ _ _ _ env _ _    -> env

instance ITraversable (List name ty) where

  isequence = \case
    Insert name ty soe env v -> pure (Insert name ty soe env v)
    Lookup name ty item pf et et' test mmatch mmiss ->
      Lookup name ty item pf et et' test <$> mmatch <*> sequenceA mmiss
    ClearList name env -> pure (ClearList name env)
    Remove name ty item pf env v -> pure (Remove name ty item pf env v)
    ForEach name ty item pf env env' mbody -> ForEach name ty item pf env env' <$> mbody

ident :: Prod r String Token String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing

tok :: String -> Prod r String Token ()
tok s = tokenMatch $ \case
  Identifier n | s == n -> Just ()
  _ -> Nothing

toks :: String -> Prod r String Token ()
toks s = go (words s)
  where
    go = \case
      [] -> empty
      [w] -> tok w
      (w:ws) -> tok w *> go ws

listEffectParser :: forall name ty
                  . (KnownSymbol name, KnownType ty)
                 => EffectParser (List name ty)
listEffectParser = EffectParser Proxy $ \value code -> mdo

  toplevel <- ruleChoice
    [ mkInsert <$> (tok "insert" *> value)
               <*> (tok "at" *> startOrEnd <* tok "of" <* listName <* token Newline)

    , mkRemove <$> (toks "remove each" *> ident)
               <*> (tok "matching" *> value <*
                    tok "from" <* listName <* token Newline)

    , mkRemoveAll <$ (toks "remove all items from" *> listName <* token Newline)

    , mkFor <$> (toks "for each" *> ident <* tok "in"
                 <* listName <* token "do" <* token Newline)
            <*> code
    ]

  let myListName = tokenMatch $ \case
        Identifier n | n == symbolVal (Proxy @name) -> Just ()
        _ -> Nothing

  listName <- rule myListName

  startOrEnd <- ruleChoice
    [ tok "start" $> Start
    , tok "end"   $> End
    ]

  pure toplevel

mkInsert :: KnownType ty => TypedValue -> StartOrEnd -> EnvCodeOfEffect (List name ty) code
mkInsert v soe = EnvCodeOfEffect $ \env -> withEnvironment env $ do
  Insert Proxy typeProxy soe env <$> atType v typeProxy

mkRemove :: forall ty name code. KnownType ty
         => String
         -> TypedValue
         -> EnvCodeOfEffect (List name ty) code
mkRemove n v = EnvCodeOfEffect $ \env -> do
  SomeSymbol name <- pure (someSymbolVal n)
  let ty = typeProxy @ty
  case lookupEnv' name env of
    Found' _ _ -> Errs $ Left ["Variable " ++ n ++ " is already defined, so it cannot be used here."]
    Absent' pf -> recallIsAbsent pf $ withEnvironment (BindingProxy name ty env) $
      Remove Proxy ty name pf env <$> atType v BooleanType

mkRemoveAll :: EnvCodeOfEffect (List name ty) code
mkRemoveAll = EnvCodeOfEffect (pure . ClearList Proxy)

mkFor :: KnownType ty => String -> EnvCodeOf code -> EnvCodeOfEffect (List name ty) code
mkFor n (EnvCodeOf body) = EnvCodeOfEffect $ \env -> do
  SomeSymbol name <- pure (someSymbolVal n)
  case lookupEnv' name env of
    Found' {} -> Errs $ Left ["Variable " ++ n ++ " is already defined, so it cannot be used here."]
    Absent' pf -> withEnvironment env $ recallIsAbsent pf $ do
      let env' = BindingProxy name typeProxy env
      ForEach Proxy typeProxy name pf env env' <$> body env'
{-
listEffectParser :: forall name ty
                  . (KnownSymbol name, KnownType ty)
                 => EffectParser (List name ty)
listEffectParser = EffectParser Proxy $ \(env :: EnvironmentProxy env) code_ -> do
  -- All list effect commands have the form of a line followed by the list name,
  -- e.g. "insert 42 at start of myList"
  -- Check if the first few tokens are the start of a list command. If so,
  -- scan ahead to the token before the last end-of-line and check that it
  -- is the name of this list.
  lookAhead (foldr1 (<|>) (map (foldr1 (>>) . map tok_) listStarts))
  lookAhead $ do
    many (satisfy (\t -> t /= Identifier name && t /= Newline && t /= "do"))
    tok_ (Identifier name)
    (eol <|> tok_ "do")

  pInsert env code_
    <|> pRemoveSome env code_
    <|> pRemoveAll env code_
    <|> pFor env code_
    <|> pWith env code_
    <?> ("list command for " ++ name)

  where
    name = symbolVal (Proxy @name)
    listStarts = [ ["insert"]
                 , ["remove"]
                 , ["for", "each"]
                 , ["with", "first"]
                 ]

    -- insert VALUE at start of LISTNAME
    -- insert VALUE at end of LISTNAME
    pInsert :: EnvironmentProxy env
            -> (forall env'. EnvironmentProxy env' -> Parser (Eval (code env')))
            -> Parser (List name ty code env)
    pInsert (env :: EnvironmentProxy env) _ = withEnvironment env $ do
      tok_ "insert"
      v <- value_ @ty @env EmptyContext
      tok_ "at"
      let start = do
            tok_ "start" >> tok_ "of" >> tok_ (Identifier name) >> eol
            pure (Insert Proxy typeProxy env v)
          end = do
            tok_ "end" >> tok_ "of" >> tok_ (Identifier name) >> eol
            pure (Insert Proxy typeProxy env v) -- FIXME
      (start <|> end <?> "insertion style")

    -- remove each item matching VALUE from LISTNAME
    pRemoveSome :: EnvironmentProxy env
                -> (forall env'. EnvironmentProxy env' -> Parser (Eval (code env')))
                -> Parser (List name ty code env)
    pRemoveSome (env :: EnvironmentProxy env) _ = withEnvironment env $ do
      -- 'try' because this has a prefix overlap with pRemoveAll
      try (tok_ "remove" >> tok_ "each")
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "matching"
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail ("a variable named '" <> itemStr <> "' is already defined")
          Absent' pf -> recallIsAbsent pf $ do
            vtoks <- manyTill anyToken (tok_ "from")
            let env' = bindNameEnv item (typeProxy @ty) pf env
            v <- nest (parseValueFromTokens env' EmptyContext BooleanType vtoks)
            tok_ (Identifier name) >> eol
            pure (Remove Proxy (typeProxy @ty) item pf env v)

    -- remove all items from LISTNAME
    pRemoveAll :: EnvironmentProxy env
               -> (forall env'. EnvironmentProxy env' -> Parser (Eval (code env')))
               -> Parser (List name ty code env)
    pRemoveAll env _ = do
      tok_ "remove" >> tok_ "all" >> tok_ "items"
        >> tok_ "from" >> tok_ (Identifier name) >> eol
      pure (ClearList Proxy env)

    -- for each item in LISTNAME do CODE
    pFor :: forall env code
          . EnvironmentProxy env
         -> (forall env'. EnvironmentProxy env' -> Parser (Eval (code env')))
         -> Parser (List name ty code env)
    pFor env code_ = do
      tok_ "for" >> tok_ "each"
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "in" >> tok_ (Identifier name) >> tok_ "do"
      some eol
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail ("a variable named " ++ itemStr ++ " is already defined")
          Absent' pf -> recallIsAbsent pf $ do
            let env' = bindNameEnv item (typeProxy @ty) pf env
            body <- code_ env'
            pure (ForEach Proxy (typeProxy @ty) item pf env env' body)

    -- with first item matching VALUE in LISTNAME do CODE
    -- with first item matching VALUE in LISTNAME do CODE else CODE
    pWith :: forall env code
           . EnvironmentProxy env
          -> (forall env'. EnvironmentProxy env' -> Parser (Eval (code env')))
          -> Parser (List name ty code env)
    pWith env code_ = withEnvironment env $ do
      tok_ "with" >> tok_ "first"
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "matching"
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail ("a variable named " ++ itemStr ++ " is already defined")
          Absent' pf -> recallIsAbsent pf $ do
            let env' = bindNameEnv item (typeProxy @ty) pf env
            v <- value_ @'BooleanT @( '(item, ty) ': env) EmptyContext
            tok_ "in" >> tok_ (Identifier name)
            tok_ "do"
            many eol
            lookAhead (tok_ Indent)
            match <- code_ env'
            -- Else branch is optional for void return type
            let elseParser = do
                  tok_ "else" >> many eol >> lookAhead (tok_ Indent)
                  Just <$> code_ env
            miss <- elseParser <|> pure Nothing
            pure (Lookup Proxy (typeProxy @ty) item pf env' env v match miss)
-}
