{-# language RecursiveDo, DataKinds, ImpredicativeTypes #-}
module Language.Code.Parser
  ( parseCode
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  ) where

import FractalStream.Prelude

import Language.Parser hiding (many)
import Language.Typecheck
import Language.Value
import Language.Value.Parser
import Language.Code
import Language.Parser.Tokenizer
import Language.Code.Typecheck

------------------------------------------------------
-- Main function for parsing Code
------------------------------------------------------

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env splices input = do
  ParsedCode c <- first Left (parse (codeGrammar splices) (tokenizeWithIndentation input))
  case c env of TC x -> first Right x

------------------------------------------------------
-- Code grammar
------------------------------------------------------

codeGrammar :: forall r
             . Splices
            -> Grammar r (Prod r ParsedCode)
codeGrammar splices = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  block <- ruleChoice
    [ check (tcBlock <$> (token Indent *> many toplevel <* token Dedent)) <?> "indented statements"
    -- Grammar hack so that Let statements slurp up the remainder
    -- of the block into their scope.
    , check (
        ((\xs x -> tcBlock (xs ++ [x]))
         <$> (token Indent *> many toplevel)
         <*> (letStatement <* token Dedent)) <?> "indented statements")
    ]

  letStatement <- rule $
    check (
     tcLet <$> ident
           <*> (colon *> typ)
           <*> (token LeftArrow *> value <* nl)
           <*> (check (tcBlock <$> blockTail)) <?> "variable initialization")

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* nl
    , check
      (tcIfThenElse <$> (token If *> value <* (colon <* nl))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , check
      (tcWhile splices
        <$> (lit "while" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "while loop"
    , check
      (tcDoWhile splices
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "while" *> value <* nl)) <?> "repeat...while loop"
    , check
      (tcUntil splices
        <$> (lit "until" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "until loop"
    , check
      (tcDoUntil splices
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "until" *> value <* nl)) <?> "repeat...until loop"
    , effect <?> "extended operation"
    ]

  upTo <- rule (lit "up" *> lit "to" *> value <* token TimesKeyword)

  elseIf <- ruleChoice
    [ ((token Else *> colon *> nl) *> block) <?> "else clause"
    , check
      (tcIfThenElse <$> ((token Else *> token If) *> value <* (colon <* nl))
                    <*> block
                    <*> elseIf) <?> "else if clause"
    , pure (ParsedCode $ \env -> withEnvironment env $ pure NoOp) <?> "end of block"
    ]

  let lit = token . Identifier

  simpleStatement <- ruleChoice
    [ check
      (tcSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , check
      (tcIterate splices
        <$> (lit "iterate" *> ident <* token RightArrow)
        <*> value
        <*> ((lit "while" $> True) <|> (lit "until" $> False))
        <*> value
        <*> optional upTo)
    , check ((\_ _ -> pure NoOp) <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammar splices

  effect <- ruleChoice
    [ toplevelDrawCommand
    , listCommand
    ]

  toplevelDrawCommand <- ruleChoice
    [ (tok "draw" *> drawCommand <* nl)
    , (tok "use" *> penCommand <* nl)
    , (tok "erase" *> eraseCommand <* nl)
    ]

  eraseCommand <- ruleChoice [check (pure tcClear)]

  drawCommand <- ruleChoice
    [ check
      (tcDrawPoint <$> ((tok "point" *> tok "at") *> value))
    , check
      (tcDrawCircle <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "circle" *> tok "at") *> value)
                    <*> ((tok "with" *> tok "radius") *> value))
    , check
      (tcDrawRect <$> (isJust <$> optional (tok "filled"))
                  <*> ((tok "rectangle" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    , check
      (tcDrawLine <$> ((tok "line" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ check (tcSetFill   <$> (value <* (tok "for" *> tok "fill")))
    , check (tcSetStroke <$> (value <* (tok "for" *> strokeOrLine)))
    ]

  startOrEnd <- ruleChoice
    [ tok "start" $> Start
    , tok "end" $> End ]

  listCommand <- ruleChoice
    [ check (tcListInsert
      <$> (tok "insert" *> value <* tok "at")
      <*> (startOrEnd <* tok "of")
      <*> (ident <* nl))
    , check (tcListRemoveSome
      <$> (tok "remove" *> tok "each" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "from" *> ident <* nl))
    , check (tcListRemoveAll
      <$> (tok "remove" *> tok "all" *> tok "items" *> tok "from" *> ident <* nl))
    , check (tcListFor
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block))
    , check (tcListWith
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block)
      <*> optional (tok "else" *> colon *> nl *> block))
    ]

  colon <- rule (token Colon <?> ":")
  nl <- rule (token Newline <?> "end of line")

  pure toplevel

check :: Prod r CheckedCode -> Prod r ParsedCode
check = withSourceRange
      . fmap @_ @CheckedCode (\c sr -> ParsedCode (\env -> withEnvironment env $ c sr env))
