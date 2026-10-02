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
import Language.Value.Typecheck (FunctionContext(..), FunctionInfo(..), noFunctions, reservedIdentifiers)
import Language.Code
import Language.Parser.Tokenizer
import Language.Parser.SourceRange (SourceRange(..), Pos(..))
import Language.Code.Typecheck
import Language.Code.Dual (tcSolveCompound, tcCriticalCompound)

import Data.Char (isSpace)
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
  let (defs, mainLines) = splitDefines input
      scriptRow = Map.fromList (zip [0 ..] (map fst mainLines))
      toks = mapPositions (\(Pos r c) -> Pos (Map.findWithDefault r r scriptRow) c)
                          (tokenizeWithIndentation (unlines (map snd mainLines)))
  (fctx, cfs) <- buildFunctionContext env (valueSplices splices) defs
  ParsedCode c <- first Left (parse (codeGrammar splices { functionContext = fctx
                                                         , codeFunctions = cfs })
                                    toks)
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
    , check
      (tcSolve
        <$> (lit "solve" *> ident <* token RightArrow)
        <*> value
        <*> optional (lit "within" *> value)
        <*> optional upTo) <?> "solve statement"
    , check
      (tcSolveCompound
        <$> (lit "solve" *> ident <* token RightArrow)
        <*> compoundCall
        <*> optional (lit "within" *> value)
        <*> optional upTo) <?> "solve statement (compound function)"
    , check
      (tcPreimage
        <$> (lit "preimage" *> ident <* token RightArrow)
        <*> value
        <*> (lit "of" *> value)
        <*> optional (lit "within" *> value)
        <*> optional upTo) <?> "preimage statement"
    , check
      (tcCritical
        <$> (lit "critical" *> ident <* token RightArrow)
        <*> value
        <*> optional (lit "within" *> value)
        <*> optional upTo) <?> "critical statement"
    , check
      (tcCriticalCompound
        <$> (lit "critical" *> ident <* token RightArrow)
        <*> compoundCall
        <*> optional (lit "within" *> value)
        <*> optional upTo) <?> "critical statement (compound function)"
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

-- | A top-level variable captured where a function is defined, as (original
-- name, snapshot name, type). The body reads the snapshot, so later
-- assignments to the original don't affect the function.
type Snapshot = (String, String, SomeType)

-- | Split a script into its @define@ blocks and the remaining main source.
--
-- * A define block is a column-0 @define@ line plus the indented or blank
--   lines after it.
-- * Each define carries its header row (0-based, for error positions) and
--   snapshots of the top-level variables declared before it.
-- * In the main source, the block is replaced by the snapshots' @Let@s, so
--   they run before any later mutation.
-- * Each main-source line carries its row in the script (the define's row,
--   for snapshot lines).
splitDefines :: String -> ([(Int, String, [Snapshot])], [(Int, String)])
splitDefines input = go (0 :: Int) [] [] [] (zip [0 ..] (lines input))
  where
    go _ _     defs mainLs [] = (reverse defs, reverse mainLs)
    go i decls defs mainLs ((row, l) : ls)
      | isTopLevelDefine l =
          let (body, rest) = span (isBodyLine . snd) ls
              mk (dn, tystr, sty) =
                let sn = "fsSnap_" ++ show i ++ "_" ++ dn
                in ((dn, sn, sty), (row, sn ++ " : " ++ tystr ++ " <- " ++ dn))
              (snaps, snapLines) = unzip (map mk (reverse decls))
          in go (i + 1) decls ((row, unlines (l : map snd body), snaps) : defs)
                (reverse snapLines ++ mainLs) rest
      | otherwise =
          let decls' = case (startsWithSpace l, parseTopLevelDecl l) of
                         (False, Just d) -> d : decls
                         _               -> decls
          in go i decls' defs ((row, l) : mainLs) ls

    isTopLevelDefine l = case words l of
      ("define" : _) -> not (startsWithSpace l)
      _              -> False

    isBodyLine l = null (trim l) || startsWithSpace l

    startsWithSpace (c : _) = isSpace c
    startsWithSpace []      = False

-- | Recognize a top-level variable declaration line @name : type <- …@ and
-- return its name, the (source) type string, and the parsed type.
parseTopLevelDecl :: String -> Maybe (String, String, SomeType)
parseTopLevelDecl line =
  case break ((== LeftArrow) . baseToken) (tokenize line) of
    (lhs, _arrow : _) -> case break ((== Colon) . baseToken) lhs of
      (nameToks, _colon : tyToks)
        | [Identifier nm] <- map baseToken nameToks
        , Right ty <- parse typeGrammar tyToks
        -> Just (nm, unwords (map (tokenStr . baseToken) tyToks), withType ty SomeType)
      _ -> Nothing
    _ -> Nothing
  where
    tokenStr = \case
      Identifier s -> s
      OpenParen    -> "("
      CloseParen   -> ")"
      _            -> ""

-- | Extend an environment with (name, type) bindings, skipping names already
-- present (shadowing is rejected by the typechecker anyway).
extendEnv :: EnvironmentProxy env -> [(String, SomeType)] -> SomeEnvironment
extendEnv env [] = SomeEnvironment env
extendEnv env ((nm, SomeType ty) : rest) = case someSymbolVal nm of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' pf -> recallIsAbsent pf $ extendEnv (BindingProxy name ty env) rest
    Found' _ _ -> extendEnv env rest

-- | Parse define blocks in order, so each one can call the earlier ones.
-- Expression functions go into a 'FunctionContext' over @env@; compound
-- functions into a separate table.
buildFunctionContext
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> [(Int, String, [Snapshot])]
  -> Either (Either ParseError TCError) (FunctionContext, Map String CompoundFunction)
buildFunctionContext env vsplices = go Map.empty Map.empty
  where
    go exprAcc compAcc [] = Right (FunctionContext env exprAcc, compAcc)
    go exprAcc compAcc ((row, blk, snaps) : rest) = do
      result <- parseOneDefine env vsplices exprAcc compAcc snaps row blk
      case result of
        Left  fi -> go (Map.insert (fiName fi) fi exprAcc) compAcc rest
        Right cf -> go exprAcc (Map.insert (cfName cf) cf compAcc) rest

-- | A semantic (non-parse) error raised while resolving a definition.
defError :: SourceRange -> String -> Either (Either ParseError TCError) a
defError sr msg = Left (Right (Advice sr msg))

-- | Parse one define block.
--
-- * A body @slot <- expression@ gives a 'FunctionInfo' (inlined as an
--   expression).
-- * Any other body gives a 'CompoundFunction' (spliced as statements).
--
-- The slot is the function's name or @result@.
parseOneDefine
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> Map String FunctionInfo
  -> Map String CompoundFunction
  -> [Snapshot]
  -> Int
  -> String
  -> Either (Either ParseError TCError) (Either FunctionInfo CompoundFunction)
parseOneDefine env vsplices exprFuncs compFuncs snaps headerRow blk = case lines blk of
  [] -> Left (Left defineParseError)
  (headerLine : rawBodyLines) -> do
    let headerToks  = shiftTokens headerRow 0 (tokenize headerLine)
        headerRange = foldMap tokenSourceRange headerToks
        defErr      = defError headerRange
    (name, params) <- first Left (parse defineHeaderGrammar headerToks)
    when (Map.member name exprFuncs || Map.member name compFuncs) $
      defErr ("The function `" ++ name ++ "` is already defined.")
    checkReserved defErr "a function name" name
    mapM_ (checkReserved defErr "a parameter name" . fst) params
    let counts = Map.fromListWith (+) [ (p, 1 :: Int) | (p, _) <- params ]
    case Map.keys (Map.filter (> 1) counts) of
      (dup : _) -> defErr ("The parameter `" ++ dup ++ "` appears more than once.")
      []        -> pure ()
    let indent     = commonIndent rawBodyLines
        bodyLines  = map (drop indent) rawBodyLines
        -- Body tokens are produced from the dedented body alone; shift them
        -- back to their position in the whole script.
        bodyRow    = headerRow + 1
        freshes    = [ freshArgName name i | i <- [0 .. length params - 1] ]
        paramMap   = Map.fromList (zip (map fst params) freshes)
        -- Rename references to top-level variables to their definition-site
        -- snapshots, and use the snapshots as the function's environment.
        snapRename = Map.fromList [ (dn, sn)  | (dn, sn, _)  <- snaps ]
        defSiteEnv = extendEnv env [ (sn, sty) | (_, sn, sty) <- snaps ]
        renameVars = paramMap `Map.union` snapRename
        compNames  = Map.keysSet compFuncs
        nonBlank   = [ (k, l) | (k, l) <- zip [0 ..] bodyLines, not (null (trim l)) ]
    case nonBlank of
      [(k, single)] -> do
        body <- first Left (parseDefineBody env exprFuncs compNames vsplices name renameVars
                                (shiftTokens (bodyRow + k) indent (tokenize single)))
        Right (Left FunctionInfo { fiName        = name
                                 , fiParams      = params
                                 , fiFreshParams = freshes
                                 , fiBody        = body
                                 , fiDefEnv      = defSiteEnv })
      _ -> do
        let resultName = freshResultName name
            renameMap  = Map.insert name resultName
                       . Map.insert "result" resultName
                       $ renameVars
        body <- first Left (parseCompoundBody env vsplices exprFuncs compFuncs renameMap
                                  (shiftTokens bodyRow indent
                                     (tokenizeWithIndentation (unlines bodyLines))))
        Right (Right CompoundFunction { cfName        = name
                                      , cfParams      = params
                                      , cfFreshParams = freshes
                                      , cfResultName  = resultName
                                      , cfBody        = body })
  where
    checkReserved defErr role n
      | n `Set.member` reservedIdentifiers =
          defErr ("`" ++ n ++ "` is a reserved word and can't be used as "
                  ++ role ++ ".")
      | otherwise = Right ()

-- | Parse a compound function body as a code block, with parameters and the
-- result slot renamed to fresh internal names.
parseCompoundBody
  :: forall env
   . EnvironmentProxy env
  -> ValueSplices
  -> Map String FunctionInfo
  -> Map String CompoundFunction
  -> Map String String
  -> [SRToken]
  -> Either ParseError ParsedCode
parseCompoundBody env vsplices exprFuncs compFuncs renameMap bodyToks =
  let splices = noSplices { valueSplices    = vsplices
                          , functionContext = FunctionContext env exprFuncs
                          , codeFunctions   = compFuncs }
      toks = renameTokens renameMap bodyToks
  in parse (codeGrammar splices) toks

freshResultName :: String -> String
freshResultName fn = "[fn-res " ++ fn ++ "]"

-- | Grammar for a define header, @define name(p1, p2 : T, ...):@. Each
-- parameter may carry a type annotation.
defineHeaderGrammar :: forall r. Grammar r (Prod r (String, [(String, Maybe SomeType)]))
defineHeaderGrammar = do
  typ <- typeGrammar
  param <- rule ((,) <$> ident
                     <*> optional (token Colon *> fmap (`withType` SomeType) typ))
  params <- rule (((:) <$> param <*> many (token Comma *> param)) <|> pure [])
  rule (((,) <$> (tok "define" *> ident)
             <*> (token OpenParen *> params <* token CloseParen <* token Colon))
        <?> "a function definition like `define f(x):`")

-- | Parse an expression function body, @slot <- expression@. Parameters are
-- renamed to fresh internal names, so the body cannot capture (or be captured
-- by) variables at the call site.
parseDefineBody
  :: forall env
   . EnvironmentProxy env
  -> Map String FunctionInfo
  -> Set String
  -> ValueSplices
  -> String
  -> Map String String
  -> [SRToken]
  -> Either ParseError ParsedValue
parseDefineBody env funcs compNames vsplices fnName renameMap bodyToks =
  case break ((== LeftArrow) . baseToken) bodyToks of
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

-- | The common leading-space indentation of a block of lines (ignoring blank
-- lines). Stripping it lets an indented function body be tokenized as a
-- top-level block.
commonIndent :: [String] -> Int
commonIndent ls = case map indentOf (filter (not . null . trim) ls) of
  []      -> 0
  indents -> minimum indents
  where indentOf = length . takeWhile (== ' ')

-- | Move tokens by a number of rows and columns.
shiftTokens :: Int -> Int -> [SRToken] -> [SRToken]
shiftTokens dr dc = mapPositions (\(Pos r c) -> Pos (r + dr) (c + dc))

mapPositions :: (Pos -> Pos) -> [SRToken] -> [SRToken]
mapPositions f = map $ \t -> t { tokenSourceRange = case tokenSourceRange t of
  NoSourceRange   -> NoSourceRange
  SourceRange a b -> SourceRange (f a) (f b) }

