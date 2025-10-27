module Language.Parser
  ( optional
  , void
  , (<|>)
  , empty
  , many
  , some
  , (<&>)
  , ($>)
  , (<$)

  , ParseError(..)
  , ppFullError

  -- * Functions that behave as the like-named ones
  -- from Text.Earley, except also track source ranges
  , token
  , tokenMatch
  , satisfy
  , ruleChoice
  , rule
  , Prod(..)
  , withSourceRange
  , (<?>)
  , parse
  , ident
  , tok

   -- * Re-exports from Text.Earley
  , P.Grammar

  ) where

import FractalStream.Prelude

import qualified Data.Set as Set
import qualified Text.Earley as P

import Language.Parser.Tokenizer
import Language.Parser.SourceRange

------------------------------------------------------
-- Versions of types and combinators from Text.Earley,
-- modified so that they also track source ranges.
------------------------------------------------------

newtype Prod r a = Prod { runProd :: P.Prod r String SRToken (SourceRange, a) }
  deriving Functor

instance Applicative (Prod r) where
  pure = Prod . pure . (NoSourceRange,)
  Prod mf <*> Prod ma = Prod $
    (\(sr, f) (sr', x) -> (sr <> sr', f x)) <$> mf <*> ma

instance Alternative (Prod r) where
  empty = Prod empty
  Prod x <|> Prod y = Prod (x <|> y)
  many (Prod p) = Prod (sequence <$> many p)
  some (Prod p) = Prod (sequence <$> some p)

instance Semigroup a => Semigroup (Prod r a) where
  Prod px <> Prod py = Prod $
    (\(sr,x) (sr',y) -> (sr <> sr', x <> y)) <$> px <*> py

instance Monoid a => Monoid (Prod r a) where
  mempty = Prod (pure (NoSourceRange, mempty))

token :: Token -> Prod r Token
token bt = satisfy (== bt)

tokenMatch :: (Token -> Maybe a) -> Prod r a
tokenMatch f = satisfy (isJust . f) <&> \t -> case f t of
    Just x  -> x
    Nothing -> error "impossible"

satisfy :: (Token -> Bool) -> Prod r Token
satisfy f = Prod ((\(SRToken bt sr) -> (sr, bt)) <$> P.satisfy (f . baseToken))

ident :: Prod r String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing

tok :: String -> Prod r ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  _ -> Nothing

ruleChoice :: [Prod r a] -> P.Grammar r (Prod r a)
ruleChoice = rule . asum

rule :: Prod r a -> P.Grammar r (Prod r a)
rule = fmap Prod . P.rule . runProd

-- | A named production (used for reporting expected things).
infixr 0 <?>
(<?>) :: Prod r a -> String -> Prod r a
Prod p <?> name = Prod (p P.<?> name)

parse :: (forall r. P.Grammar r (Prod r a)) -> [SRToken] -> Either ParseError a
parse p input = case P.fullParses (P.parser (fmap snd . runProd <$> p)) input of
    ([], r)  -> Left (NoParse `onReport` r)
    ([x], _) -> pure x
    (_, r)   -> Left (AmbiguousParse `onReport` r)
  where
    onReport f P.Report{..} = f (toPos position) (Set.fromList expected)
    toPos i = case mconcat (take (i + 1) (map tokenSourceRange input)) of
      NoSourceRange   -> Nothing
      SourceRange _ e -> Just e

withSourceRange :: Prod r (SourceRange -> a) -> Prod r a
withSourceRange (Prod p) = Prod $ (\(sr, f) -> (sr, f sr)) <$> p

data ParseError
  = NoParse (Maybe Pos) (Set String)
  | AmbiguousParse (Maybe Pos) (Set String)
  deriving Show

instance PrettyPrint ParseError where
  pp = (:[]) . \case
    NoParse _ prods -> "I got stuck and couldn't parse the input here" ++
      expectingClause prods
    AmbiguousParse _ prods -> "I got stuck because the grammar is ambiguous here" ++
      expectingClause prods

expectingClause :: Set String -> String
expectingClause prods = case unsnoc (Set.toList prods) of
  Nothing  -> "."
  Just ([], x) -> ", where I was expecting " ++ x ++ "."
  Just ([x'], x) -> ", where I was expecting either " ++ x' ++ " or " ++ x ++ "."
  Just (xs, x)   -> ", where I was expecting one of " ++ intercalate ", " xs ++ ", or " ++ x ++ "."

instance HasErrorLocation ParseError where
  errorLocation = \case
    NoParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    AmbiguousParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp

ppFullError :: (PrettyPrint t, HasErrorLocation t) => t -> String -> String
ppFullError err input =
  let msg = pp err
      context = pp (Indicated (errorLocation err) input)
  in unlines (context ++ [""] ++ msg)
