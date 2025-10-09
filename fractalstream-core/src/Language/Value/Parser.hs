{-# language RecursiveDo #-}
module Language.Value.Parser
  ( parseValue
  , parseTypedValue
  , parseValueFromTokens
  , parseTypedValueFromTokens
  , tokenize
  , tokenizeWithIndentation
  , valueGrammar
  , valueGrammarWithNoSplices
  , atType
  , ruleChoice
  , typeGrammar
  , TypedValue(..)
  , Splices
  , Errs(..)
  , Alternative(..)
  , tokenMatch
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Value
import Language.Parser
import Data.Color

type Splices = Map String TypedValue

ruleChoice :: [Prod r e t a] -> Grammar r (Prod r e t a)
ruleChoice = rule . asum

parseValue :: forall env ty. (KnownEnvironment env, KnownType ty)
           => Splices
           -> String
           -> Either String (Value '(env, ty))
parseValue splices = parseValueFromTokens splices . tokenize

parseValueFromTokens :: forall env ty. (KnownEnvironment env, KnownType ty)
                     => Splices
                     -> [Token]
                     -> Either String (Value '(env, ty))
parseValueFromTokens splices tokens = do
  tv <- parseTypedValueFromTokens splices tokens
  case tv `atType` typeProxy of
    Errs (Left errs) -> Left (init . unlines . Set.toList . Set.fromList $ errs)
    Errs (Right v)   -> pure v

parseTypedValue :: Map String TypedValue
                -> String
                -> Either String TypedValue
parseTypedValue splices = parseTypedValueFromTokens splices . tokenize

parseTypedValueFromTokens :: Map String TypedValue
                          -> [Token]
                          -> Either String TypedValue
parseTypedValueFromTokens splices tokens =
  case fullParses (parser (valueGrammar splices)) tokens of
    ([], r)   -> Left ("no parse: " ++ show r)
    ([tv], _) -> pure tv
    (_, r)    -> Left ("ambiguous parse: " ++ show r)


newtype TypedValue = TypedValue (forall env ty. KnownEnvironment env => TypeProxy ty -> Errs (Value '(env, ty)))

-- | Try to interpret the value at the given type, inserting casts if needed.
atType :: KnownEnvironment env
       => TypedValue
       -> TypeProxy ty
       -> Errs (Value '(env, ty))
atType v@(TypedValue f) ty = case ty of
  RealType    -> (I2R <$> atType v IntegerType) <|> f RealType
  ComplexType -> (R2C <$> atType v RealType)    <|> f ComplexType
  _           -> f ty

typeError :: TypeProxy ty -> String -> String -> Errs a
typeError expected thing thingType = Errs $
  Left ["I expected a value of " ++ showType expected ++ " type here, " ++
         "but " ++ thing ++ " has " ++ thingType ++ " type."]

nameError :: String -> Errs a
nameError n = Errs $ Left ["No variable named " ++ n ++ " is in scope here."]

valueGrammarWithNoSplices :: forall r. Grammar r (Prod r String Token TypedValue)
valueGrammarWithNoSplices = valueGrammar Map.empty

valueGrammar :: forall r
              . Splices
             -> Grammar r (Prod r String Token TypedValue)
valueGrammar splices = mdo

  let toplevel = cast

  cast <- ruleChoice
    [ mkCast <$> ite <*> (token Colon *> typ)
    , ite
    ]

  ite <- ruleChoice
    [ mkITE <$> (token If *> scalar) <*> (token Then *> scalar) <*> (token Else *> ite)
    , scalar
    ]

  scalar <- rule pair

  pair <- ruleChoice
    [ mkPair <$> (ite <* token Comma) <*> pair
    , pOr
    ]

  pOr <- ruleChoice
    [ mkOr <$> (pAnd <* token Or_) <*> pOr <?> "disjunction"
    , pAnd
    ]

  pAnd <- ruleChoice
    [ mkAnd <$> (pNot <* token And_) <*> pAnd <?> "conjunction"
    , pNot
    ]

  pNot <- ruleChoice
    [ mkNot <$> (token Not_ *> arith)
    , comparison
    ]

  comparison <- ruleChoice
    [ mkEql <$> (arith <* token Equal)    <*> arith
    , mkNEq <$> (arith <* token NotEqual) <*> arith
    , mkLT  <$> (arith <* token LessThan) <*> arith
    , mkGT  <$> (arith <* token GreaterThan) <*> arith
    , mkGTE <$> (arith <* token GreaterThanOrEqual) <*> arith
    , mkLTE <$> (arith <* token LessThanOrEqual) <*> arith
    , arith
    ]

  let arith = addOrSub

  addOrSub <- ruleChoice
    [ mkAdd <$> (addOrSub <* token Plus)  <*> mulOrDiv
    , mkSub <$> (addOrSub <* token Minus) <*> mulOrDiv
    , mulOrDiv
    ]

  mulOrDiv <- ruleChoice
    [ mkMul <$> (mulOrDiv <* token Times) <*> negated
    , mkMul <$> concatenatedFunAps <*> funAp
    , mkMul <$> concatenatedAtoms <*> noFunPower
    , mkDiv <$> (mulOrDiv <* token Divide) <*> negated
    , mkIDiv <$> (mulOrDiv <* token IntegerDivide) <*> negated
    , negated
    ]

  concatenatedAtoms <- ruleChoice
    [ mkMul <$> concatenatedAtoms <*> noFunPower
    , noFunPower
    , (funAp $> TypedValue (\_ -> Errs $ Left ["To avoid ambiguity when using implicit mulitplication, functions must go to the left of other values."]))
    ]

  concatenatedFunAps <- ruleChoice
    [ mkMul <$> concatenatedFunAps <*> funAp
    , mkMul <$> concatenatedAtoms <*> atom
    , funAp
    , atom
    ]

  funAp <- ruleChoice
    [ mkCommonFun
      <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n commonFunctions;
                               _ -> Nothing }))
      <*> atomOrFunAp

    , mkRealFun
      <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n realFunctions;
                               _ -> Nothing }))
      <*> atomOrFunAp

    , mkComplexFun
      <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n complexFunctions;
                               _ -> Nothing }))
      <*> atomOrFunAp
    ]

  negated <- ruleChoice
    [ mkNeg <$> (token Minus *> negated)
    , power
    ]

  power <- ruleChoice
    [ mkPow <$> (atomOrFunAp <* token Caret) <*> power
    , atomOrFunAp
    ]

  noFunPower <- ruleChoice
    [ mkPow <$> (atom <* token Caret) <*> power
    , atom
    ]

  atomOrFunAp <- rule (funAp <|> atom)

  atom <- ruleChoice
    [ simpleAtom
    , token OpenParen   *> toplevel <* token CloseParen
    , token OpenBracket *> toplevel <* token CloseBracket
    , token OpenBrace   *> toplevel <* token CloseBrace
    ]

  simpleAtom <- ruleChoice
    [ mkScalar "`true`"  BooleanType True  <$ token True_
    , mkScalar "`false`" BooleanType False <$ token False_
    , mkScalar "𝑖" ComplexType (0.0 :+ 1.0) <$ token I
    , mkScalar "𝑒" RealType (exp 1.0) <$ token Euler
    , mkScalar "π" RealType pi <$ token Pi
    , (\n -> mkScalar (show n) IntegerType (fromIntegral n))
      <$> tokenMatch (\case { NumberI n -> Just n; _ -> Nothing })
    , (\n -> mkScalar (show n) RealType n)
      <$> tokenMatch (\case { NumberF n -> Just n; _ -> Nothing })
    , mkAbs <$> (token Bar *> arith <* token Bar)

    , tokenMatch $ \case
        Identifier n -> Map.lookup n splices
        _ -> Nothing

    , var
    , color
    ]

  var <- rule $ fmap mkVar . tokenMatch $ \case
        Identifier n | not (reserved n) -> Just n;
          _ -> Nothing

  color <- ruleChoice
    [ fmap (\c -> mkScalar (show c) ColorType c) . tokenMatch $ \case
        Identifier n -> Map.lookup n colors
        _ -> Nothing

    , mkDark <$> (token (Identifier "dark") *> atom)
    , mkLight <$> (token (Identifier "light") *> atom)
    , mkInvert <$> (token (Identifier "invert") *> atom)
    , mkBlend <$> (token (Identifier "blend") *> (token OpenParen *> arith))
              <*> (token Comma *> atom)
              <*> (token Comma *> atom <* token CloseParen)
    , mkRGB <$> (token (Identifier "rgb") *> (token OpenParen *> arith))
            <*> (token Comma *> arith)
            <*> (token Comma *> arith <* token CloseParen)
    ]

  typ <- typeGrammar

  let reserved = (`Set.member` reservedWords)
      reservedWords = Set.fromList
        [ "dark", "light", "invert", "blend", "rgb", "mod"]
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet splices

  pure toplevel

mkCast :: TypedValue -> FSType -> TypedValue
mkCast v tgt = TypedValue $ \ty -> withType tgt $ \tgtTy ->
  case sameHaskellType ty tgtTy of
    Nothing -> Errs $ Left ["Conversion to type " ++ showType tgtTy ++ " used in a context "
                           ++ "where the type " ++ showType ty ++ " was expected."]
    Just Refl -> case ty of
      RealType -> (atType v RealType) <|> (I2R <$> atType v IntegerType)
      ComplexType -> atType v ComplexType
                 <|> (R2C <$> atType v RealType)
                 <|> (R2C . I2R <$> atType v IntegerType)
      _ -> atType v ty

mkPair :: TypedValue -> TypedValue -> TypedValue
mkPair lhs rhs = TypedValue $ \ty -> case ty of
  PairType t1 t2 -> PairV ty <$> atType lhs t1 <*> atType rhs t2
  _ -> typeError ty "a tuple" "pair"

mkITE :: TypedValue -> TypedValue -> TypedValue -> TypedValue
mkITE cond yes no = TypedValue $ \ty ->
  ITE ty <$> atType cond BooleanType <*> atType yes ty <*> atType no ty

mkOr, mkAnd :: TypedValue -> TypedValue -> TypedValue
mkOr lhs rhs = TypedValue $ \case
  BooleanType -> Or <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> typeError ty "a disjunction" "boolean"

mkAnd lhs rhs = TypedValue $ \case
  BooleanType -> And <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> typeError ty "a conjunction" "boolean"

mkNot :: TypedValue -> TypedValue
mkNot arg = TypedValue $ \case
  BooleanType -> Not <$> atType arg BooleanType
  ty -> typeError ty "logical negation" "boolean"

mkEql, mkNEq, mkLT, mkLTE, mkGT, mkGTE :: TypedValue -> TypedValue -> TypedValue

mkEql lhs rhs = TypedValue $ \case
  BooleanType -> asum
    [ Eql RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , Eql ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , Eql IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , Eql BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> typeError ty "the arguments to =" "some equatable"

mkNEq lhs rhs = TypedValue $ \case
  BooleanType -> asum
    [ NEq RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , NEq ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , NEq IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , NEq BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> typeError ty "the arguments to ≠" "some equatable"

mkLT lhs rhs = TypedValue $ \case
  BooleanType -> asum
    [ LTF <$> atType lhs RealType    <*> atType rhs RealType
    , LTI <$> atType lhs IntegerType <*> atType rhs IntegerType
    ]
  ty -> typeError ty "<" "some ordered"

mkGT lhs rhs = TypedValue $ \case
  BooleanType -> asum
    [ LTF <$> atType rhs RealType    <*> atType lhs RealType
    , LTI <$> atType rhs IntegerType <*> atType lhs IntegerType
    ]
  ty -> typeError ty ">" "some ordered"

mkGTE lhs rhs = TypedValue $ \case
  BooleanType -> asum
    [ Not <$> (LTF <$> atType lhs RealType    <*> atType rhs RealType)
    , Not <$> (LTI <$> atType lhs IntegerType <*> atType rhs IntegerType)
    ]
  ty -> typeError ty "≥" "some ordered"

mkLTE lhs rhs =  TypedValue $ \case
  BooleanType -> asum
    [ Not <$> (LTF <$> atType rhs RealType    <*> atType lhs RealType)
    , Not <$> (LTI <$> atType rhs IntegerType <*> atType lhs IntegerType)
    ]
  ty -> typeError ty "≤" "some ordered"

-- | Do an arithmetic operation, but try to keep conversions at the outermost
-- layer. For example, if we want to parse "1 + 2" at ComplexT, we want the
-- addition to happen over integers with the conversion to complex happening last.
-- To do this, we'll try to interpret "1 + 2" as a integer expression followed by
-- a conversion. If that fails, we'll try it as a real expression followed by a
-- conversion. And finally if that fails, we'll try it as a complex expression.
mkArith :: String
        -> (forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT))
        -> (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))
        -> (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT))
        -> TypedValue
        -> TypedValue
        -> TypedValue
mkArith op makeI makeF makeC lhs rhs = TypedValue $ \ty -> case ty of
  IntegerType -> makeI <$> atType lhs IntegerType <*> atType rhs IntegerType
  RealType    -> (I2R <$> (makeI <$> atType lhs IntegerType <*> atType rhs IntegerType))
             <|> (makeF <$> atType lhs RealType <*> atType rhs RealType)
  ComplexType -> (R2C . I2R <$> (makeI <$> atType lhs IntegerType <*> atType rhs IntegerType))
             <|> (R2C <$> (makeF <$> atType lhs RealType <*> atType rhs RealType))
             <|> (makeC <$> atType lhs ComplexType <*> atType rhs ComplexType)
  _ -> typeError ty op "numeric"

mkAdd, mkSub, mkMul, mkDiv, mkIDiv, mkPow :: TypedValue -> TypedValue -> TypedValue

mkAdd = mkArith "+" AddI AddF AddC
mkSub = mkArith "-" SubI SubF SubC
mkMul = mkArith "*" MulI MulF MulC
mkPow = mkArith "^" PowI PowF PowC

mkDiv lhs rhs = TypedValue $ \ty -> case ty of
  IntegerType -> Errs $ Left ["For integer division, use the // operator."]
  RealType    -> DivF <$> atType lhs RealType <*> atType rhs RealType
  ComplexType -> (R2C <$> (DivF <$> atType lhs RealType <*> atType rhs RealType))
             <|> (DivC <$> atType lhs ComplexType <*> atType rhs ComplexType)
  _ -> typeError ty "/" "real or complex"

mkIDiv lhs rhs = TypedValue $ \ty -> case ty of
  IntegerType -> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  RealType    -> I2R <$> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  ComplexType -> R2C . I2R <$> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  _ -> typeError ty "//" "integer"

mkNeg :: TypedValue -> TypedValue
mkNeg x = TypedValue $ \ty -> case ty of
  IntegerType -> NegI <$> atType x IntegerType
  RealType    -> (I2R . NegI <$> atType x IntegerType)
             <|> (NegF <$> atType x RealType)
  ComplexType -> (R2C . I2R . NegI <$> atType x IntegerType)
             <|> (R2C . NegF <$> atType x RealType)
             <|> (NegC <$> atType x ComplexType)
  _ -> typeError ty "negation" "numeric"

mkScalar :: String -> TypeProxy ty -> HaskellType ty -> TypedValue
mkScalar name ty' v = TypedValue $ \ty -> case sameHaskellType ty ty' of
  Just Refl -> pure (Const $ Scalar ty v)
  Nothing   -> typeError ty name (showType ty')

mkAbs :: TypedValue -> TypedValue
mkAbs x = TypedValue $ \ty -> case ty of
  IntegerType -> AbsI <$> atType x IntegerType
  RealType -> (AbsF <$> atType x RealType)
          <|> (AbsC <$> atType x ComplexType)
  _ -> typeError ty "|·|" "real or integer"

mkVar :: String -> TypedValue
mkVar n = TypedValue $ \ty -> case someSymbolVal n of
  SomeSymbol name -> case lookupEnv name ty (envProxy Proxy) of
    Found pf -> pure (Var name ty pf)
    WrongType (SomeType ty') -> typeError ty n (showType ty')
    Absent _ -> nameError n

data CommonFun = CommonFun
  (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))
  (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT))

mkCommonFun :: (String, CommonFun) -> TypedValue -> TypedValue
mkCommonFun (name, CommonFun makeF makeC) x = TypedValue $ \ty -> case ty of
  RealType    -> makeF <$> atType x RealType
  ComplexType -> (R2C . makeF <$> atType x RealType)
             <|> (makeC <$> atType x ComplexType)
  _ -> typeError ty name "real or complex"

mkRealFun :: (String, RealFun) -> TypedValue -> TypedValue
mkRealFun (name, RealFun makeF) x = TypedValue $ \ty -> case ty of
  RealType -> makeF <$> atType x RealType
  _ -> typeError ty name "real"

mkComplexFun :: (String, ComplexFun) -> TypedValue -> TypedValue
mkComplexFun (name, ComplexFun resultTy makeC) x = TypedValue $ \ty -> case resultTy of
  RealType -> case ty of
    RealType -> makeC <$> atType x ComplexType
    ComplexType -> R2C . makeC <$> atType x ComplexType
    _ -> typeError ty name "real"
  ComplexType -> case ty of
    ComplexType -> makeC <$> atType x ComplexType
    _ -> typeError ty name "complex"
  _ -> typeError ty name (showType resultTy)

mkInvert, mkDark, mkLight :: TypedValue -> TypedValue
mkDark c = TypedValue $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType black)
               <$> atType c ColorType
  _ -> typeError ty "a darkening operation" "color"

mkLight c = TypedValue $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType white)
               <$> atType c ColorType
  _ -> typeError ty "a lightening operation" "color"

mkInvert c = TypedValue $ \ty -> case ty of
  ColorType -> InvertRGB <$> atType c ColorType
  _ -> typeError ty "a color inversion operation" "color"

mkBlend, mkRGB :: TypedValue -> TypedValue -> TypedValue -> TypedValue
mkBlend t c1 c2 = TypedValue $ \ty -> case ty of
  ColorType -> Blend <$> atType t RealType
                     <*> atType c1 ColorType
                     <*> atType c2 ColorType
  _ -> typeError ty "a blend operation" "color"

mkRGB r g b = TypedValue $ \ty -> case ty of
  ColorType -> RGB <$> atType r RealType
                   <*> atType g RealType
                   <*> atType b RealType
  _ -> typeError ty "an RGB value" "color"

tokenMatch :: (t -> Maybe a) -> Prod r e t a
tokenMatch f = satisfy (isJust . f) <&> \t -> case f t of
    Just x  -> x
    Nothing -> error "impossible"

colors :: Map String Color
colors = Map.fromList
  [ ("red", red), ("green", green), ("blue", blue)
  , ("black", black), ("white", white), ("grey", grey), ("gray", grey)
  , ("orange", orange), ("yellow", yellow), ("purple", purple), ("violet", violet) ]

realFunctions :: Map String RealFun
realFunctions = Map.fromList
  [ ("arccos",  RealFun ArccosF)
  , ("arcsin",  RealFun ArcsinF)
  , ("arctan",  RealFun ArctanF)
  , ("acos",    RealFun ArccosF)
  , ("asin",    RealFun ArcsinF)
  , ("atan",    RealFun ArctanF)
  , ("arccosh", RealFun ArccoshF)
  , ("arcsinh", RealFun ArcsinhF)
  , ("arctanh", RealFun ArctanhF)
  , ("acosh",   RealFun ArccoshF)
  , ("asinh",   RealFun ArcsinhF)
  , ("atanh",   RealFun ArctanhF)
  ]

commonFunctions :: Map String CommonFun
commonFunctions = Map.fromList
  [ ("exp",  CommonFun ExpF   ExpC)
  , ("log",  CommonFun LogF   LogC)
--  , ("abs", CommonFun AbsF AbsC)
  , ("sqrt", CommonFun SqrtF  SqrtC)
  , ("cos",  CommonFun CosF   CosC)
  , ("sin",  CommonFun SinF   SinC)
  , ("tan",  CommonFun TanF   TanC)
  , ("cosh", CommonFun CoshF  CoshC)
  , ("sinh", CommonFun SinhF  SinhC)
  , ("tanh", CommonFun TanhF  TanhC)
  ]

data RealFun = RealFun (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))

data ComplexFun where
  ComplexFun :: forall ty
              . TypeProxy ty
             -> (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, ty))
             -> ComplexFun

complexFunctions :: Map String ComplexFun
complexFunctions = Map.fromList
  [ ("re",  ComplexFun RealType ReC)
  , ("Re",  ComplexFun RealType ReC)
  , ("im",  ComplexFun RealType ImC)
  , ("Im",  ComplexFun RealType ImC)
  , ("arg", ComplexFun RealType ArgC)
  , ("Arg", ComplexFun RealType ArgC)
  , ("conj", ComplexFun ComplexType ConjC)
  , ("Conj", ComplexFun ComplexType ConjC)
  ]

types :: Map String FSType
types = Map.fromList
  [ ("R", RealT), ("ℝ", RealT), ("Real", RealT)
  , ("Z", IntegerT), ("ℤ", IntegerT), ("Int", IntegerT), ("Integer", IntegerT)
  , ("C", ComplexT), ("ℂ", ComplexT), ("Complex", ComplexT)
  , ("Bool", BooleanT), ("Boolean", BooleanT)
  , ("Color", ColorT)
  ]

typeGrammar :: forall r. Grammar r (Prod r String Token FSType)
typeGrammar = mdo

  toplevel <- rule $ tokenMatch $ \case
    Identifier t -> Map.lookup t types
    _ -> Nothing

  pure toplevel
