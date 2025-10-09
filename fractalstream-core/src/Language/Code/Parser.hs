{-# language RecursiveDo, DataKinds #-}
module Language.Code.Parser
  ( parseCode
  , EffectParser(..)
  , EffectParsers(..)
  , EffectParsers_(..)
  --, uParseCode
  --, uCode
  , Errs(..)
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser hiding (many)
import Language.Value
import Language.Value.Parser hiding (parseValueFromTokens)
import Language.Code

import qualified Data.Set as Set
import Data.Indexed.Functor (FIX)

newtype EnvCode effs = EnvCode
  (forall env. EnvironmentProxy env -> Errs (Code effs env))

parseCode :: forall effs env
           . EffectParsers effs
          -> EnvironmentProxy env
          -> Splices
          -> String
          -> Either String (Code effs env)
parseCode eps env splices input = do
  EnvCode c <- parseEnvCode eps splices input
  case c env of
    Errs (Left errs) -> Left (init . unlines . Set.toList . Set.fromList $ errs)
    Errs (Right c')  -> pure c'

parseEnvCode :: forall effs
              . EffectParsers effs
             -> Splices
             -> String
             -> Either String (EnvCode effs)
parseEnvCode eps splices input = do
  let toks = tokenizeWithIndentation input
  case fullParses (parser (codeGrammar eps splices)) toks of
    ([], r)   -> Left ("no parse: " ++ show r)
    ([c], _)  -> pure c
    (_, r)    -> Left ("ambiguous parse: " ++ show r)

ident :: Prod r String Token String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing



codeGrammar :: forall r effs
             . EffectParsers effs
            -> Splices
            -> Grammar r (Prod r String Token (EnvCode effs))
codeGrammar (EP effectParsers) splices = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  block <- ruleChoice
    [ (mkBlock <$> (token Indent *> many toplevel <* token Dedent)) <?> "indented statements"
    -- Grammar hack so that Let statements slurp up the remainder
    -- of the block into their scope.
    , ((\xs x -> mkBlock (xs ++ [x]))
      <$> (token Indent *> many toplevel)
      <*> (letStatement <* token Dedent)) <?> "indented statements"
    ]

  letStatement <- rule $
    (mkLet <$> ident
           <*> (token Colon *> typ)
           <*> (token LeftArrow *> value <* token Newline)
           <*> (mkBlock <$> blockTail)) <?> "variable initialization"

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* token Newline
    , (mkIfThenElse <$> (token If *> value <* (token Then <* token Newline))
                   <*> block
                   <*> elseIf) <?> "if statement"
    , (mkWhile <$> (lit "while" *> value <* token Newline)
              <*> block) <?> "while loop"
    , (mkDoWhile <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "while" *> value <* token Newline))
    , (mkUntil <$> (lit "until" *> value <* token Newline)
              <*> block) <?> "until loop"
    , (mkDoUntil <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "until" *> value <* token Newline))
    , effect <?> "extended operation"
    ]

  elseIf <- ruleChoice
    [ ((token Else *> token Newline) *> block) <?> "else clause"
    , (mkIfThenElse <$> ((token Else *> token If) *> value <* (token Then <* token Newline))
                   <*> block
                   <*> elseIf) <?> "else if clause"
    , pure noOp <?> "end of block"
    ]

  let lit = token . Identifier
      noOp = EnvCode $ \env -> withEnvironment env $ pure NoOp

  simpleStatement <- ruleChoice
    [ (mkSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , (noOp <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammar splices

  let parseEffectFrom :: forall effs'
                       . EffectParsers_ effs' effs
                      -> Grammar r (Prod r String Token (EnvCode effs))
      parseEffectFrom NoEffs = rule empty
      parseEffectFrom (ParseEff (EffectParser eff grammar) etc) = do
        let convert :: EnvCode effs -> EnvCodeOf (FIX (CodeF effs))
            convert (EnvCode f) = EnvCodeOf f

        r <- grammar value (convert <$> block)
        rs <- parseEffectFrom etc
        rule ((mkEffect eff <$> r) <|> rs)

  effect <- parseEffectFrom effectParsers

  pure toplevel

atEnv :: EnvironmentProxy env -> EnvCode eff -> Errs (Code eff env)
atEnv env (EnvCode f) = f env

mkBlock :: [EnvCode eff] -> EnvCode eff
mkBlock body = EnvCode $ \env ->
  withEnvironment env $ do
    Block <$> traverse (atEnv env) body

mkLet :: String -> FSType -> TypedValue -> EnvCode eff -> EnvCode eff
mkLet n t v c = EnvCode $ \env -> withType t $ \ty ->
  withEnvironment env $ do
    -- Get a proof that `name` is not already bound in the environment `env`.
    -- Make an extended environment env' that binds name to ty
    -- Evaluate c in this extended environment.
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv name ty env of
      Found _ -> Errs $ Left [n ++ " is already bound in this environment"]
      WrongType _ -> Errs $ Left [n ++ " is already bound in this environment"]
      Absent absent -> recallIsAbsent absent $ do
        let pf = bindName name ty absent
        val <- atType v ty
        let env' = BindingProxy name ty env
        Let pf name val <$> atEnv env' c

mkSet :: String -> TypedValue -> EnvCode eff
mkSet n v = EnvCode $ \env ->
  withEnvironment env $ do
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv' name env of
      Found' ty pf -> Set pf name <$> atType v ty
      Absent' _ -> Errs $ Left [n ++ " is not in scope here."]

mkWhile :: TypedValue -> EnvCode eff -> EnvCode eff
mkWhile cond body = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> atType cond BooleanType <*> atEnv env body)
               <*> pure NoOp

mkDoWhile :: EnvCode eff -> TypedValue -> EnvCode eff
mkDoWhile body cond = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> atType cond BooleanType <*> atEnv env body

mkUntil :: TypedValue -> EnvCode eff -> EnvCode eff
mkUntil cond body = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body)
               <*> pure NoOp

mkDoUntil :: EnvCode eff -> TypedValue -> EnvCode eff
mkDoUntil body cond = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body

mkIfThenElse :: TypedValue -> EnvCode eff -> EnvCode eff -> EnvCode eff
mkIfThenElse cond yes no = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> atEnv env yes
               <*> atEnv env no

mkEffect :: HasEffect eff effs
         => Proxy eff
         -> EnvCodeOfEffect eff (FIX (CodeF effs))
         -> EnvCode effs
mkEffect eff (EnvCodeOfEffect f) = EnvCode $ \env -> withEnvironment env $
  Effect eff Proxy <$> f env
