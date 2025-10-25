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
           <*> (token Colon *> typ)
           <*> (token LeftArrow *> value <* token Newline)
           <*> (check (tcBlock <$> blockTail)) <?> "variable initialization")

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* token Newline
    , check
      (tcIfThenElse <$> (token If *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , check
      (tcWhile <$> (lit "while" *> value <* token Newline)
               <*> block) <?> "while loop"
    , check
      (tcDoWhile <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "while" *> value <* token Newline))
    , check
      (tcUntil <$> (lit "until" *> value <* token Newline)
               <*> block) <?> "until loop"
    , check
      (tcDoUntil <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "until" *> value <* token Newline))
    , effect <?> "extended operation"
    ]

  elseIf <- ruleChoice
    [ ((token Else *> token Newline) *> block) <?> "else clause"
    , check
      (tcIfThenElse <$> ((token Else *> token If) *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "else if clause"
    , pure (ParsedCode $ \env -> withEnvironment env $ pure NoOp) <?> "end of block"
    ]

  let lit = token . Identifier

  simpleStatement <- ruleChoice
    [ check
      (tcSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , check ((\_ _ -> pure NoOp) <$ lit "pass") <?> "pass"
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
      <*> (ident <* token Newline))
    , check (tcListRemoveSome
      <$> (tok "remove" *> tok "each" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "from" *> ident <* token Newline))
    , check (tcListRemoveAll
      <$> (tok "remove" *> tok "all" *> tok "items" *> tok "from" *> ident <* token Newline))
    , check (tcListFor
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block))
    , check (tcListWith
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block)
      <*> optional (tok "else" *> token Newline *> block))
    ]
  pure toplevel

check :: Prod r CheckedCode -> Prod r ParsedCode
check = withSourceRange
      . fmap @_ @CheckedCode (\c sr -> ParsedCode (\env -> withEnvironment env $ c sr env))
