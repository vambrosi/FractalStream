{-# language OverloadedStrings #-}
module Language.Parser
  (
   tokenize
  , tokenizeWithIndentation
  , Token(..)
  , BaseToken(..)
  , SourceRange(..)
  , Errs(..)
  , optional
  , void
  , (<|>)
  , empty
  , many
  , some
  , (<&>)
  , ($>)
  , (<$)

  , token
  , tokenMatch
  , satisfy
  , ruleChoice
  , rule
  , ProdSR(..)
  , withSourceRange
  , (<?>)
  , parser

   -- * Re-exports from Text.Earley
  , P.Grammar
  , P.fullParses

  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import Data.Char
import Data.List (isPrefixOf, sortOn)
import Data.Ord
import qualified Text.Earley as P

identChar :: Char -> Bool
identChar c = (isAlphaNum c && not (c `elem` superscripts) && (c /= '⁻'))
              || c == '_'

opTokens :: [(String, BaseToken)]
opTokens = sortOn (\x -> (Down (length (fst x)), x)) $
  [ ("+", Plus), ("-", Minus), ("*", Times)
  , ("//", IntegerDivide), ("/", Divide)
  , ("^", Caret), ("|", Bar), ("(", OpenParen), (")", CloseParen)
  , ("[", OpenBracket), ("]", CloseBracket)
  , ("{", OpenBrace), ("}", CloseBrace)
  , (",", Comma), (":", Colon), (";", Semicolon)
  , (">", GreaterThan), ("<", LessThan)
  , ("==", Equal), ("=", Equal)
  , (">=", GreaterThanOrEqual), ("<=", LessThanOrEqual)
  , ("≥", GreaterThanOrEqual), ("≤", LessThanOrEqual)
  , ("!=", NotEqual), ("=/=", NotEqual), ("≠", NotEqual)
  , ("->", RightArrow), ("<-", LeftArrow)
  , ("⭢", RightArrow), ("⭠", LeftArrow)
  , ("→", RightArrow), ("←", LeftArrow)
  ]

longestMatchingOperator :: SRString -> Maybe (Token, SRString)
longestMatchingOperator cs =
  listToMaybe [ ( Token t (mconcat $ map snd $ take (length s) cs)
                , drop (length s) cs)
              | (s, t) <- opTokens, s `isPrefixOf` (map fst cs) ]

wordlikeTokens :: Map String BaseToken
wordlikeTokens = Map.fromList
  [ ("if", If), ("then", Then), ("else", Else)
  , ("e", Euler), ("𝑒", Euler)
  , ("pi", Pi), ("π", Pi)
  , ("i", I), ("𝑖", I)
  , ("true", True_), ("false", False_)
  , ("or", Or_), ("and", And_), ("not", Not_)
  ]
data TokenGroup
  = Group [Token] [TokenGroup]
  | Single [Token]
  deriving (Eq, Show)

toTok :: TokenGroup -> [Token]
toTok (Single s) = s ++ [Token Newline NoSourceRange]
toTok (Group s xs) = [Token Indent NoSourceRange] ++ s ++
                     [Token Newline NoSourceRange] ++
                     concatMap toTok xs ++ [Token Dedent NoSourceRange]

tokenize :: String -> [Token]
tokenize = tokenize' .
  zipWith (\i c -> let p = Pos 0 i in (c, SourceRange p p)) [0..]

tokenize' :: SRString -> [Token]
tokenize' = \case
  -- Done!
  [] -> []

  -- Skip whitespace
  (c:cs) | isSpace (fst c) -> tokenize' cs

  -- Skip comments
  (('#',_):_) -> []

  -- Tokenize positive numbers
  cs@(d:_)
    | isDigit (fst d) ->
        let ds = takeWhile (isDigit . fst) cs
        in case dropWhile (isDigit . fst) cs of
          (('.',sr) : cs'@(d' : _))
            | isDigit (fst d') ->
                let ds' = ds <> [('.', sr)] <> takeWhile (isDigit . fst) cs'
                in Token (NumberF (read $ map fst ds')) (mconcat $ map snd ds')
                   : tokenize' (dropWhile (isDigit . fst) cs')
          cs' -> Token (NumberI (read $ map fst ds)) (mconcat $ map snd ds)
                 : tokenize' cs'

  -- Tokenize the ⁻, ⁰, ¹, ², ³, ⁴, ⁵, ⁶, ⁷, ⁸, ⁹ superscripts
  (('⁻',sr):cs@(d:_))
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in Token Caret NoSourceRange
            : Token (NumberI (negate $ superscriptNumber (map fst ds)))
                    (sr <> mconcat (map snd ds))
            : tokenize' cs'
  cs@(d:_)
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in Token Caret NoSourceRange
            : Token (NumberI (superscriptNumber (map fst ds)))
                    (mconcat (map snd ds))
            : tokenize' cs'

  -- Tokenize special operators
  cs | Just (tok, cs') <- longestMatchingOperator cs
       -> tok : tokenize' cs'

  -- Tokenize identifiers
  cs@(c:_) | isAlpha (fst c) && not (fst c `elem` superscripts) && (fst c /= '⁻') ->
               let ds = takeWhile (identChar . fst) cs
                   cs' = dropWhile (identChar . fst) cs
                   tok = case Map.lookup (map fst ds) wordlikeTokens of
                     Just t  -> t
                     Nothing -> Identifier (map fst ds)
               in Token tok (mconcat (map snd ds)) : tokenize' cs'

  -- Otherwise, grab a junk character
  ((c, sr):cs) -> Token (Junk c) sr : tokenize' cs

superscripts :: String
superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹"

superscriptNumber :: String -> Integer
superscriptNumber = go 0
  where
    go acc = let next i = go (10 * acc + i)
             in \case
      [] -> acc
      ('⁰':xs) -> next 0 xs
      ('¹':xs) -> next 1 xs
      ('²':xs) -> next 2 xs
      ('³':xs) -> next 3 xs
      ('⁴':xs) -> next 4 xs
      ('⁵':xs) -> next 5 xs
      ('⁶':xs) -> next 6 xs
      ('⁷':xs) -> next 7 xs
      ('⁸':xs) -> next 8 xs
      ('⁹':xs) -> next 9 xs
      _ -> error "should be impossible"

type SRString = [(Char, SourceRange)]

tokenizeWithIndentation :: String -> [Token]
tokenizeWithIndentation
         = ([Token Indent NoSourceRange] ++)
         . (++ [Token Dedent NoSourceRange])
         . concatMap toTok
         . block
         . map observeSpaces
         . filter (not . all (isSpace . fst))
         . map dropComments
         . map (\(r, xs) -> map (\(c, x) -> let p = Pos r c
                                            in (x, SourceRange p p)) xs)
         . zip [0..]
         . map (zip [0..])
         . lines
  where
    dropComments :: SRString -> SRString
    dropComments = \case
      []      -> []
      (('#',_):_) -> []
      (c:cs)  -> c : dropComments cs

    observeSpaces :: SRString -> (Int, SRString)
    observeSpaces s = (length (takeWhile (isSpace . fst) s)
                      , dropWhile (isSpace . fst) s)

    -- Dedent all lines by n spaces
    extract :: Int -> [(Int, SRString)] -> ([TokenGroup], [(Int, SRString)])
    extract n ss =
      let ss'  = takeWhile ((>= n) . fst) ss
          ss'' = dropWhile ((>= n) . fst) ss
      in (block [(m - n, x) | (m,x) <- ss'], ss'')

    block :: [(Int, SRString)] -> [TokenGroup]
    block = \case
      [] -> []
      ((n,s) : ss)
        | n == 0 -> Single (tokenize' s) : block ss
        | n > 0 ->
            let (is, ss') = extract n ss
            in Group (tokenize' s) is : block ss'
      _ -> error "bad indent"

data BaseToken
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | IntegerDivide
  | Caret
  | Bar
  | Identifier String
  | Junk Char
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Colon
  | Semicolon
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  | Indent
  | Dedent
  | Newline
  | If
  | Then
  | Else
  | Euler
  | Pi
  | I
  | True_
  | False_
  | Or_
  | And_
  | Not_
  | LeftArrow
  | RightArrow
  deriving (Eq, Ord, Show)

data SourceRange
  = SourceRange Pos Pos
  | NoSourceRange
  deriving (Eq, Show)

data Pos = Pos { posRow :: Int, posCol :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup SourceRange where
  NoSourceRange <> r = r
  r <> NoSourceRange = r
  SourceRange s e <> SourceRange s' e' =
    SourceRange (min s s') (max e e')

instance Monoid SourceRange where
  mempty = NoSourceRange

data Token = Token
  { baseToken :: BaseToken
  , tokenSourceRange :: SourceRange
  }
  deriving (Show)

-- | CAUTION! Sketchy Eq instance ahead!
instance Eq Token where
  t1 == t2 = baseToken t1 == baseToken t2

instance IsString BaseToken where fromString = Identifier

newtype Errs a = Errs (Either [String] a)
  deriving (Show, Functor, Applicative, Monad)

instance Alternative Errs where
  Errs xs <|> Errs ys = Errs $ case (xs, ys) of
    (Right v, _) -> Right v
    (_, Right v) -> Right v
    (Left xer, Left yer) -> Left (xer ++ yer)
  empty = Errs (Left [])

newtype ProdSR r e a = ProdSR { runProdSR :: P.Prod r e Token (SourceRange, a) }
  deriving Functor

instance Applicative (ProdSR r e) where
  pure = ProdSR . pure . (NoSourceRange,)
  ProdSR mf <*> ProdSR ma = ProdSR $
    (\(sr, f) (sr', x) -> (sr <> sr', f x)) <$> mf <*> ma

instance Alternative (ProdSR r e) where
  empty = ProdSR empty
  ProdSR x <|> ProdSR y = ProdSR (x <|> y)
  many (ProdSR p) = ProdSR (sequence <$> many p)
  some (ProdSR p) = ProdSR (sequence <$> some p)

instance Semigroup a => Semigroup (ProdSR r e a) where
  ProdSR px <> ProdSR py = ProdSR $
    (\(sr,x) (sr',y) -> (sr <> sr', x <> y)) <$> px <*> py

instance Monoid a => Monoid (ProdSR r e a) where
  mempty = ProdSR (pure (NoSourceRange, mempty))

token :: BaseToken -> ProdSR r e BaseToken
token bt = satisfy (== bt)

tokenMatch :: (BaseToken -> Maybe a) -> ProdSR r e a
tokenMatch f = satisfy (isJust . f) <&> \t -> case f t of
    Just x  -> x
    Nothing -> error "impossible"

satisfy :: (BaseToken -> Bool) -> ProdSR r e BaseToken
satisfy f = ProdSR ((\(Token bt sr) -> (sr, bt)) <$> P.satisfy (f . baseToken))

ruleChoice :: [ProdSR r e a] -> P.Grammar r (ProdSR r e a)
ruleChoice = rule . asum

rule :: ProdSR r e a -> P.Grammar r (ProdSR r e a)
rule = fmap ProdSR . P.rule . runProdSR


-- | A named production (used for reporting expected things).
infixr 0 <?>
(<?>) :: ProdSR r e a -> e -> ProdSR r e a
ProdSR p <?> e = ProdSR (p P.<?> e)

parser :: (forall r. P.Grammar r (ProdSR r e a)) -> P.Parser e [Token] a
parser p = P.parser (fmap snd . runProdSR <$> p)

withSourceRange :: ProdSR r e (SourceRange -> a) -> ProdSR r e a
withSourceRange (ProdSR p) = ProdSR $ (\(sr, f) -> (sr, f sr)) <$> p
