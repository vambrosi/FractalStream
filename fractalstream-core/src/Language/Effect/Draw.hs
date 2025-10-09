{-# language OverloadedStrings, RecursiveDo #-}
module Language.Effect.Draw
  ( type Draw
  , Draw_(..)
  , type DrawCommand
  , drawEffectParser
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Parser
import Language.Value.Parser
import Language.Effect
import Data.Indexed.Functor

type Draw = Draw_ Value_

type DrawCommand = Draw_ ConcreteValue NoCode '[]

data ConcreteValue :: Environment -> FSType -> Exp Type
type instance Eval (ConcreteValue env t) = HaskellType t

data NoCode :: Environment -> Exp Type
type instance Eval (NoCode env) = ()

data Draw_ (value :: Environment -> FSType -> Exp Type)
           (code :: Environment -> Exp Type)
           (env :: Environment) where

  -- | draw point at POINT
  DrawPoint :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env  ('Pair 'RealT 'RealT))
            -> Draw_ value code env

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  DrawCircle :: forall env code value
              . EnvironmentProxy env
             -> Bool
             -> Eval (value env 'RealT)
             -> Eval (value env ('Pair 'RealT 'RealT))
             -> Draw_ value code env

  -- | draw line from POINT to POINT
  DrawLine :: forall env code value
            . EnvironmentProxy env
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value code env

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env code value
            . EnvironmentProxy env
           -> Bool
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value code env

  -- | use COLOR for stroke
  SetStroke :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value code env

  -- | use COLOR for fill
  SetFill :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value code env

  -- | erase
  Clear :: forall env code value
         . EnvironmentProxy env
        -> Draw_ value code env

instance IFunctor (Draw_ value) where
  type IndexProxy (Draw_ value) = EnvironmentProxy
  imap _ = \case
    DrawPoint e p -> DrawPoint e p
    DrawCircle e f r p -> DrawCircle e f r p
    DrawLine e f t -> DrawLine e f t
    DrawRect e f c1 c2 -> DrawRect e f c1 c2
    SetStroke e c -> SetStroke e c
    SetFill e c -> SetFill e c
    Clear e -> Clear e
  toIndex = \case
    DrawPoint e _ -> e
    DrawCircle e _ _ _ -> e
    DrawLine e _ _ -> e
    DrawRect e _ _ _ -> e
    SetStroke e _ -> e
    SetFill e _ -> e
    Clear e -> e

instance ITraversable (Draw_ value) where
  isequence = \case
    DrawPoint e p -> pure (DrawPoint e p)
    DrawCircle e f r p -> pure (DrawCircle e f r p)
    DrawLine e f t -> pure (DrawLine e f t)
    DrawRect e f c1 c2 -> pure (DrawRect e f c1 c2)
    SetStroke e c -> pure (SetStroke e c)
    SetFill e c -> pure (SetFill e c)
    Clear e -> pure (Clear e)

tok :: String -> Prod r String Token ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  _ -> Nothing

drawEffectParser :: EffectParser Draw
drawEffectParser = EffectParser Proxy $ \value _code -> mdo

  toplevel <- ruleChoice
    [ (tok "draw" *> drawCommand <* token Newline)
    , (tok "use" *> penCommand <* token Newline)
    , EnvCodeOfEffect (pure . Clear) <$ (tok "erase" *> token Newline)
    ]

  drawCommand <- ruleChoice
    [ mkDrawPoint <$> ((tok "point" *> tok "at") *> value)
    , mkDrawCircle <$> (isJust <$> optional (tok "filled"))
                   <*> ((tok "circle" *> tok "at") *> value)
                   <*> ((tok "with" *> tok "radius") *> value)
    , mkDrawRect <$> (isJust <$> optional (tok "filled"))
                 <*> ((tok "rectangle" *> tok "from") *> value)
                 <*> (tok "to" *> value)
    , mkDrawLine <$> ((tok "line" *> tok "from") *> value)
                 <*> (tok "to" *> value)
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ mkSetFill   <$> (value <* (tok "for" *> tok "fill"))
    , mkSetStroke <$> (value <* (tok "for" *> strokeOrLine))
    ]

  pure toplevel

mkDrawPoint :: TypedValue -> EnvCodeOfEffect Draw code
mkDrawPoint v = EnvCodeOfEffect $ \env -> withEnvironment env $
  (DrawPoint env <$> atType v (PairType RealType RealType)) <|>
  (DrawPoint env . C2R2 <$> atType v ComplexType)

mkDrawCircle :: Bool -> TypedValue -> TypedValue -> EnvCodeOfEffect Draw code
mkDrawCircle isFilled center radius = EnvCodeOfEffect $ \env -> withEnvironment env $
  DrawCircle env isFilled
    <$> atType radius RealType
    <*> (atType center (PairType RealType RealType) <|>
         (C2R2 <$> atType center ComplexType))

mkDrawRect :: Bool -> TypedValue -> TypedValue -> EnvCodeOfEffect Draw code
mkDrawRect isFilled ul lr = EnvCodeOfEffect $ \env -> withEnvironment env $
  DrawRect env isFilled
    <$> (atType ul (PairType RealType RealType) <|>
         (C2R2 <$> atType ul ComplexType))
    <*> (atType lr (PairType RealType RealType) <|>
         (C2R2 <$> atType lr ComplexType))

mkDrawLine :: TypedValue -> TypedValue -> EnvCodeOfEffect Draw code
mkDrawLine ul lr = EnvCodeOfEffect $ \env -> withEnvironment env $
  DrawLine env
    <$> (atType ul (PairType RealType RealType) <|>
         (C2R2 <$> atType ul ComplexType))
    <*> (atType lr (PairType RealType RealType) <|>
         (C2R2 <$> atType lr ComplexType))

mkSetStroke :: TypedValue -> EnvCodeOfEffect Draw code
mkSetStroke c = EnvCodeOfEffect $ \env -> withEnvironment env $
  SetStroke env <$> atType c ColorType

mkSetFill :: TypedValue -> EnvCodeOfEffect Draw code
mkSetFill c = EnvCodeOfEffect $ \env -> withEnvironment env $
  SetFill env <$> atType c ColorType

{-
drawEffectParser :: EffectParser Draw
drawEffectParser = EffectParser Proxy $
  \(env :: EnvironmentProxy env) _code -> pDrawEffect env

pDrawEffect
  :: EnvironmentProxy env
  -> Parser (Draw code env)
pDrawEffect env =
  ((pErase env
     <|> pDrawCommand env
     <|> pUseCommand env) <* eol)
  <?> "draw command"

pErase :: EnvironmentProxy env -> Parser (Draw code env)
pErase env = do
  tok_ "erase"
  pure (Clear env)

pUseCommand :: EnvironmentProxy env
            -> Parser (Draw code env)
pUseCommand env = do
  tok_ "use"
  valueTokens <- manyTill anyToken (tok_ "for")
  c <- nest (parseValueFromTokens env EmptyContext ColorType valueTokens)
  ((tok_ "fill" $> SetFill env c)
   <|> (tok_ "line" $> SetStroke env c)
   <|> (tok_ "stroke" $> SetStroke env c)
   <?> "color target")

pDrawCommand :: EnvironmentProxy env
             -> Parser (Draw code env)
pDrawCommand env = do
  tok_ "draw"
  pDrawPoint
    <|> pDrawLine
    <|> pDrawCircle False
    <|> pDrawRect False
    <|> pDrawFilled
    <?> "draw command"

  where

    pDrawPoint = withEnvironment env $ do
      tok_ "point" >> tok_ "at"
      DrawPoint env . C2R2 <$> value_ EmptyContext

    pDrawLine = withEnvironment env $ do
      tok_ "line" >> tok_ "from"
      valueTokens <- manyTill anyToken (tok_ "to")
      p1 <- C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      p2 <- C2R2 <$> value_ EmptyContext
      pure (DrawLine env p1 p2)

    pDrawCircle fill = withEnvironment env $ do
      tok_ "circle" >> tok_ "at"
      valueTokens <- manyTill anyToken (tok_ "with")
      center <- C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      tok_ "radius"
      r <- value_ EmptyContext
      pure (DrawCircle env fill r center)

    pDrawRect fill = withEnvironment env $ do
      tok_ "rectangle" >> tok_ "from"
      valueTokens <- manyTill anyToken (tok_ "to")
      p1 <- C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      p2 <- C2R2 <$> value_ EmptyContext
      pure (DrawRect env fill p1 p2)

    pDrawFilled = do
      tok_ "filled"
      pDrawCircle True <|> pDrawRect True
-}
