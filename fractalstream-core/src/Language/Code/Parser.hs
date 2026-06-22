{-# language RecursiveDo, DataKinds, ImpredicativeTypes #-}
module Language.Code.Parser
  ( parseCode
  , parseParsedCode
  , Splices(..)
  , noSplices
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  , ParsedCode(..)
  ) where

import FractalStream.Prelude

import Language.Parser hiding (many)
import Language.Typecheck
import Language.Value
import Language.Value.Parser
import Language.Value.Typecheck (FunctionContext(..), FunctionInfo(..), noFunctions)
import Language.Code
import Language.Parser.Tokenizer
import Language.Code.Typecheck

import Data.Char (isSpace)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------------------------
-- Main function for parsing Code
------------------------------------------------------

data Splices = Splices
  { codeSplices     :: Map String ParsedCode
  , valueSplices    :: Map String ParsedValue
  , functionContext :: FunctionContext
  , codeFunctions   :: Map String CompoundFunction
  }

noSplices :: Splices
noSplices = Splices Map.empty Map.empty noFunctions Map.empty

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env splices input = do
  let (defs, mainSrc) = splitDefines input
  (fctx, cfs) <- first Left (buildFunctionContext env (valueSplices splices) defs)
  ParsedCode c <- parseParsedCode (splices { functionContext = fctx
                                           , codeFunctions = cfs }) mainSrc
  case c env of TC x -> first Right x

parseParsedCode :: Splices -> String -> Either (Either ParseError TCError) ParsedCode
parseParsedCode splices input =
  first Left (parse (codeGrammar splices) (tokenizeWithIndentation input))

------------------------------------------------------
-- Code grammar
------------------------------------------------------

codeGrammar :: forall r
             . Splices
            -> Grammar r (Prod r ParsedCode)
codeGrammar Splices{..} = case functionContext of
  FunctionContext baseEnv funcs ->
    codeGrammar' baseEnv funcs codeFunctions codeSplices valueSplices

codeGrammar' :: forall r baseEnv
              . EnvironmentProxy baseEnv
             -> Map String FunctionInfo
             -> Map String CompoundFunction
             -> Map String ParsedCode
             -> ValueSplices
             -> Grammar r (Prod r ParsedCode)
codeGrammar' baseEnv funcs compoundFns codeSplices valueSplices = mdo

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
    , spliced <* nl
    , check
      (tcIfThenElse <$> (token If *> value <* (colon <* nl))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , check
      (tcWhile
        <$> (lit "while" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "while loop"
    , check
      (tcDoWhile
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "while" *> value <* nl)) <?> "repeat...while loop"
    , check
      (tcUntil
        <$> (lit "until" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "until loop"
    , check
      (tcDoUntil
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

  compoundCall <- rule $
    (,) <$> tokenMatch (\case { Identifier n -> Map.lookup n compoundFns; _ -> Nothing })
        <*> (token OpenParen *> valArgList <* token CloseParen)

  simpleStatement <- ruleChoice
    [ check
      ((\target (cf, as) -> tcSetCompound target cf as)
        <$> (ident <* token LeftArrow) <*> compoundCall) <?> "function call"
    , check
      (tcSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , check
      (tcIterate
        <$> (lit "iterate" *> ident <* token RightArrow)
        <*> value
        <*> ((lit "while" $> True) <|> (lit "until" $> False))
        <*> value
        <*> optional upTo)
    , check ((\_ _ -> pure NoOp) <$ lit "pass") <?> "pass"
    ]

  (value, valArgList) <- valueGrammar baseEnv funcs (Map.keysSet compoundFns) valueSplices

  spliced <- rule $
    token OpenSplice *>
    (tokenMatch $ \case { Identifier n -> Map.lookup n codeSplices; _ -> Nothing })
    <* token CloseSplice

  effect <- ruleChoice
    [ toplevelDrawCommand
    , listCommand
    ]

  toplevelDrawCommand <- ruleChoice
    [ (tok "draw" *> drawCommand <* nl)
    , (tok "use" *> penCommand <* nl)
    , (tok "erase" *> eraseCommand <* nl)
    , (tok "write" *> writeCommand <* nl)
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

  writeCommand <- rule $
    check (tcWrite <$> value <*> (tok "at" *> value))

  listCommand <- ruleChoice
    [ check (tcListFor
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

------------------------------------------------------
-- User-defined functions: pre-pass over the source
------------------------------------------------------

-- | Split a script into its top-level @define@ blocks and the remaining
-- ("main") source. A define block is a top-level (column-0) line whose first
-- word is @define@, together with the indented/blank lines that follow it.
splitDefines :: String -> ([String], String)
splitDefines = go [] [] . lines
  where
    go defs mainLs [] = (reverse defs, unlines (reverse mainLs))
    go defs mainLs (l : ls)
      | isTopLevelDefine l =
          let (body, rest) = span isBodyLine ls
              -- Replace the define block with blank lines in the main source so
              -- that line numbers (and therefore error positions) stay aligned.
              blanks = replicate (1 + length body) ""
          in go (unlines (l : body) : defs) (blanks ++ mainLs) rest
      | otherwise = go defs (l : mainLs) ls

    isTopLevelDefine l = case words l of
      ("define" : _) -> not (startsWithSpace l)
      _              -> False

    isBodyLine l = null (trim l) || startsWithSpace l

    startsWithSpace (c : _) = isSpace c
    startsWithSpace []      = False

-- | Parse each define block in order (so later definitions can call earlier
-- ones), collecting expression-reducible functions into a 'FunctionContext'
-- (anchored at @env@, the definition-site / script base environment) and
-- compound (statement-bodied) functions into a separate table.
buildFunctionContext
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> [String]
  -> Either ParseError (FunctionContext, Map String CompoundFunction)
buildFunctionContext env vsplices = go Map.empty Map.empty
  where
    go exprAcc compAcc [] = Right (FunctionContext env exprAcc, compAcc)
    go exprAcc compAcc (blk : rest) = do
      result <- parseOneDefine env vsplices exprAcc compAcc blk
      case result of
        Left  fi -> go (Map.insert (fiName fi) fi exprAcc) compAcc rest
        Right cf -> go exprAcc (Map.insert (cfName cf) cf compAcc) rest

-- | Parse a single define block. A body that is a single @slot <- expression@
-- becomes an expression-reducible 'FunctionInfo'; any other (multi-statement)
-- body becomes a compound 'CompoundFunction'. The slot must be the function's
-- name or the reserved word @result@.
parseOneDefine
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> Map String FunctionInfo
  -> Map String CompoundFunction
  -> String
  -> Either ParseError (Either FunctionInfo CompoundFunction)
parseOneDefine env vsplices exprFuncs compFuncs blk = case lines blk of
  [] -> Left defineParseError
  (headerLine : rawBodyLines) -> do
    (name, params) <- parseDefineHeader headerLine
    let bodyLines  = dedent rawBodyLines
        freshes    = [ freshArgName name i | i <- [0 .. length params - 1] ]
        paramMap   = Map.fromList (zip (map fst params) freshes)
        compNames  = Map.keysSet compFuncs
        nonBlank   = filter (not . null . trim) bodyLines
    case nonBlank of
      [_single] -> do
        body <- parseDefineBody env exprFuncs compNames vsplices name paramMap
                                (unlines bodyLines)
        Right (Left FunctionInfo { fiName        = name
                                 , fiParams      = params
                                 , fiFreshParams = freshes
                                 , fiBody        = body })
      _ -> do
        let resultName = freshResultName name
            renameMap  = Map.insert name resultName
                       . Map.insert "result" resultName
                       $ paramMap
        body <- parseCompoundBody env vsplices exprFuncs compFuncs renameMap
                                  (unlines bodyLines)
        Right (Right CompoundFunction { cfName        = name
                                      , cfParams      = params
                                      , cfFreshParams = freshes
                                      , cfResultName  = resultName
                                      , cfBody        = body })

-- | Parse a compound (multi-statement) function body as a code block, with
-- parameters and the result slot renamed to fresh internal names. Earlier
-- functions (expression and compound) are in scope.
parseCompoundBody
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> Map String FunctionInfo
  -> Map String CompoundFunction
  -> Map String String
  -> String
  -> Either ParseError ParsedCode
parseCompoundBody env vsplices exprFuncs compFuncs renameMap bodySrc =
  let splices = noSplices { valueSplices    = vsplices
                          , functionContext = FunctionContext env exprFuncs
                          , codeFunctions   = compFuncs }
      toks = renameTokens renameMap (tokenizeWithIndentation bodySrc)
  in parse (codeGrammar splices) toks

freshResultName :: String -> String
freshResultName fn = "[fn-res " ++ fn ++ "]"

-- | Parse a define header of the form @define name(p1, p2 : T, ...)@.
parseDefineHeader :: String -> Either ParseError (String, [(String, Maybe SomeType)])
parseDefineHeader line0 =
  case stripPrefix "define" (trim line0) of
    Nothing -> Left defineParseError
    Just afterDefine -> case break (== '(') (trim afterDefine) of
      (namePart, '(' : rest) -> case break (== ')') rest of
        (paramsPart, ')' : _) -> do
          params <- parseParams paramsPart
          Right (trim namePart, params)
        _ -> Left defineParseError
      _ -> Left defineParseError

parseParams :: String -> Either ParseError [(String, Maybe SomeType)]
parseParams s
  | null (trim s) = Right []
  | otherwise     = traverse parseParam (splitOn ',' s)

parseParam :: String -> Either ParseError (String, Maybe SomeType)
parseParam s = case break (== ':') s of
  (nm, ':' : tyStr) -> do
    ty <- parseType tyStr
    Right (trim nm, Just ty)
  (nm, _) -> Right (trim nm, Nothing)

-- | Parse the body of a define (a single @slot <- expression@). Parameter
-- references are renamed to fresh, collision-proof internal names at the token
-- level, so the resulting expression cannot capture / be captured by variables
-- at the call site.
parseDefineBody
  :: forall env
   . EnvironmentProxy env
  -> Map String FunctionInfo
  -> Set String
  -> ValueSplices
  -> String
  -> Map String String
  -> String
  -> Either ParseError ParsedValue
parseDefineBody env funcs compNames vsplices fnName renameMap bodySrc =
  case break ((== LeftArrow) . baseToken) (tokenize bodySrc) of
    (lhs, _arrow : rhs) -> case map baseToken lhs of
      [Identifier slot]
        | slot == fnName || slot == "result" ->
            parse (fst <$> valueGrammar env funcs compNames vsplices)
                  (renameTokens renameMap rhs)
      _ -> Left defineParseError
    _ -> Left defineParseError

renameTokens :: Map String String -> [SRToken] -> [SRToken]
renameTokens m = map rename
  where
    rename srt = case baseToken srt of
      Identifier n | Just n' <- Map.lookup n m -> srt { baseToken = Identifier n' }
      _ -> srt

freshArgName :: String -> Int -> String
freshArgName fn i = "[fn-arg " ++ fn ++ " " ++ show i ++ "]"

defineParseError :: ParseError
defineParseError = NoParse Nothing
  (Set.singleton "a function definition like `define f(x): result <- ...`")

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Remove the common leading-space indentation from a block of lines, so an
-- indented function body can be re-tokenized as a top-level block.
dedent :: [String] -> [String]
dedent ls = case map indentOf (filter (not . null . trim) ls) of
  []      -> ls
  indents -> map (drop (minimum indents)) ls
  where indentOf = length . takeWhile (== ' ')

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, [])       -> [a]
