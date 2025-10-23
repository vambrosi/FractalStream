{-# language RecursiveDo #-}
module Language.Value.Parser
  ( parseValue
  , parseTypedValue
  , parseValueOrList
  , parseValueFromTokens
  , parseTypedValueFromTokens
  , parseType
  , tokenize
  , tokenizeWithIndentation
  , valueGrammar
  , valueGrammarWithNoSplices
  , atType
  , typeGrammar
  , TypedValue(..)
  , Splices
  , Errs(..)
  , Alternative(..)
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Value
import Language.Parser
import Data.Color

type Splices = Map String TypedValue

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

parseType :: String -> Either String SomeType
parseType input =
  case fullParses (parser typeGrammar) (tokenize input) of
    ([], r)   -> Left ("no parse: " ++ show r)
    ([ty], _) -> withType ty (pure . SomeType)
    (_, r)    -> Left ("ambiguous parse: " ++ show r)

parseValueOrList :: forall env ty. (KnownEnvironment env, KnownType ty)
                 => Splices
                 -> String
                 -> Either String (Value '(env, ty))
parseValueOrList splices input = case typeProxy @ty of
  ListType ity -> case filter (not . (`elem` " \t\n\r")) input of
    "" -> pure (List ity [])
    _  -> parseValue splices ("[" ++ input ++ "]")
  _ -> parseValue splices input

newtype TypedValue = TypedValue
  (forall env ty. KnownEnvironment env
    => TypeProxy ty
    -> Errs (Value '(env, ty)))

-- | Try to interpret the value at the given type, inserting casts if needed.
atType :: KnownEnvironment env
       => TypedValue
       -> TypeProxy ty
       -> Errs (Value '(env, ty))
atType v@(TypedValue f) ty = case ty of
  RealType    -> (I2R <$> atType v IntegerType) <|> f RealType
  ComplexType -> (R2C <$> atType v RealType)    <|> f ComplexType
  _           -> f ty

typeError :: SourceRange -> TypeProxy ty -> String -> String -> Errs a
typeError sr thingType thing expected = Errs $
  Left ["I expected a value of " ++ expected ++ " type here, " ++
         "but " ++ thing ++ " has " ++ showType thingType ++ " type. loc=" ++ show sr]

nameError :: SourceRange -> String -> Errs a
nameError sr n = Errs $ Left ["No variable named " ++ n ++ " is in scope here. loc=" ++ show sr]

valueGrammarWithNoSplices :: forall r. Grammar r (ProdSR r String TypedValue)
valueGrammarWithNoSplices = valueGrammar Map.empty

valueGrammar :: forall r
              . Splices
             -> Grammar r (ProdSR r String TypedValue)
valueGrammar splices = mdo

  let toplevel = cast

  cast <- ruleChoice
    [ withSourceRange (mkCast <$> ite <*> (token Colon *> typ))
    , ite
    ]

  ite <- ruleChoice
    [ withSourceRange (mkITE <$> (token If *> scalar)
                             <*> (token Then *> scalar)
                             <*> (token Else *> ite))
    , scalar
    ]

  scalar <- ruleChoice [ list, pair ]

  list <- rule (withSourceRange (mkList <$> (token OpenBracket *> list' <* token CloseBracket)))

  list' <- ruleChoice
    [ (:) <$> pOr <*> many (token Comma *> pOr)
    , pure []
    ]

  pair <- ruleChoice
    [ withSourceRange (mkPair <$> (ite <* token Comma) <*> pair)
    , pOr
    ]

  pOr <- ruleChoice
    [ withSourceRange (mkOr <$> (pAnd <* token Or_) <*> pOr <?> "disjunction")
    , pAnd
    ]

  pAnd <- ruleChoice
    [ withSourceRange (mkAnd <$> (pNot <* token And_) <*> pAnd <?> "conjunction")
    , pNot
    ]

  pNot <- ruleChoice
    [ withSourceRange (mkNot <$> (token Not_ *> arith))
    , comparison
    ]

  comparison <- ruleChoice
    [ withSourceRange (mkEql <$> (arith <* token Equal)    <*> arith)
    , withSourceRange (mkNEq <$> (arith <* token NotEqual) <*> arith)
    , withSourceRange (mkLT  <$> (arith <* token LessThan) <*> arith)
    , withSourceRange (mkGT  <$> (arith <* token GreaterThan) <*> arith)
    , withSourceRange (mkGTE <$> (arith <* token GreaterThanOrEqual) <*> arith)
    , withSourceRange (mkLTE <$> (arith <* token LessThanOrEqual) <*> arith)
    , arith
    ]

  let arith = addOrSub

  addOrSub <- ruleChoice
    [ withSourceRange (mkAdd <$> (addOrSub <* token Plus)  <*> mulOrDiv)
    , withSourceRange (mkSub <$> (addOrSub <* token Minus) <*> mulOrDiv)
    , mulOrDiv
    ]

  mulOrDiv <- ruleChoice
    [ withSourceRange (mkMul <$> (mulOrDiv <* token Times) <*> negated)
    , withSourceRange (mkMul <$> concatenatedFunAps <*> funAp)
    , withSourceRange (mkMul <$> concatenatedAtoms <*> noFunPower)
    , withSourceRange (mkNeg <$> (token Minus *> concatenatedAtoms'))
    , withSourceRange (mkDiv <$> (mulOrDiv <* token Divide) <*> negated)
    , withSourceRange (mkIDiv <$> (mulOrDiv <* token IntegerDivide) <*> negated)
    , negated
    ]

  concatenatedAtoms' <- rule $ withSourceRange
    (mkMul <$> concatenatedAtoms <*> noFunPower)

  concatenatedAtoms <- ruleChoice
    [ withSourceRange (mkMul <$> concatenatedAtoms <*> noFunPower)
    , noFunPower
    , withSourceRange (funAp $> \sr -> TypedValue (\_ -> Errs $ Left ["To avoid ambiguity when using implicit mulitplication, functions must go to the left of other values. loc=" ++ show sr]))
    ]

  concatenatedFunAps <- ruleChoice
    [ withSourceRange (mkMul <$> concatenatedFunAps <*> funAp)
    , withSourceRange (mkMul <$> concatenatedAtoms <*> atom)
    , funAp
    , atom
    ]

  funAp <- ruleChoice
    [ withSourceRange (mkCommonFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n commonFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , withSourceRange (mkRealFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n realFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , withSourceRange (mkComplexFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n complexFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)
    , withSourceRange (mkMod
        <$> (token (Identifier "mod") *> token OpenParen *> arith)
        <*> (token Comma *> arith <* token CloseParen))
    ]

  negated <- ruleChoice
    [ withSourceRange (mkNeg <$> (token Minus *> negated))
    , power
    ]

  power <- ruleChoice
    [ withSourceRange (mkPow <$> (atomOrFunAp <* token Caret) <*> power)
    , atomOrFunAp
    ]

  noFunPower <- ruleChoice
    [ withSourceRange (mkPow <$> (atom <* token Caret) <*> power)
    , atom
    ]

  atomOrFunAp <- rule (funAp <|> atom)

  atom <- ruleChoice
    [ simpleAtom
    , token OpenParen   *> toplevel <* token CloseParen
    , token OpenBrace   *> toplevel <* token CloseBrace
    ]

  simpleAtom <- ruleChoice
    [ withSourceRange (mkScalar "`true`"  BooleanType True  <$ token True_)
    , withSourceRange (mkScalar "`false`" BooleanType False <$ token False_)
    , withSourceRange (mkScalar "𝑖" ComplexType (0.0 :+ 1.0) <$ token I)
    , withSourceRange (mkScalar "𝑒" RealType (exp 1.0) <$ token Euler)
    , withSourceRange (mkScalar "π" RealType pi <$ token Pi)
    , withSourceRange (
        (\n -> mkScalar (show n) IntegerType (fromIntegral n))
        <$> tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))
    , withSourceRange (
        (\n -> mkScalar (show n) RealType n)
        <$> tokenMatch (\case { NumberF n -> Just n; _ -> Nothing }))
    , withSourceRange (mkAbs <$> (token Bar *> arith <* token Bar))

    , tokenMatch $ \case
        Identifier n -> Map.lookup n splices
        _ -> Nothing

    , var
    , color
    ]

  var <- rule $ withSourceRange (
    fmap mkVar . tokenMatch $ \case
        Identifier n | not (reserved n) -> Just n;
        _ -> Nothing)

  color <- ruleChoice
    [ withSourceRange (
        fmap (\c -> mkScalar (show c) ColorType c) . tokenMatch $ \case
            Identifier n -> Map.lookup n colors
            _ -> Nothing)

    , withSourceRange (mkDark <$> (token (Identifier "dark") *> atom))
    , withSourceRange (mkLight <$> (token (Identifier "light") *> atom))
    , withSourceRange (mkInvert <$> (token (Identifier "invert") *> atom))
    , withSourceRange (
        mkBlend <$> (token (Identifier "blend") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , withSourceRange (
        mkCycle <$> (token (Identifier "cycle") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , withSourceRange (
        mkRGB <$> (token (Identifier "rgb") *> (token OpenParen *> arith))
              <*> (token Comma *> arith)
              <*> (token Comma *> arith <* token CloseParen))
    ]

  typ <- typeGrammar

  let reserved = (`Set.member` reservedWords)
      reservedWords = Set.fromList
        [ "dark", "light", "invert", "blend", "cycle", "rgb", "mod"]
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet splices

  pure toplevel

mkCast :: TypedValue -> FSType -> SourceRange -> TypedValue
mkCast v tgt sr = TypedValue $ \ty -> withType tgt $ \tgtTy ->
  case sameHaskellType ty tgtTy of
    Nothing -> Errs $ Left ["Conversion to type " ++ showType tgtTy ++ " used in a context "
                           ++ "where the type " ++ showType ty ++ " was expected. loc=" ++ show sr]
    Just Refl -> case ty of
      RealType -> (atType v RealType) <|> (I2R <$> atType v IntegerType)
      ComplexType -> atType v ComplexType
                 <|> (R2C <$> atType v RealType)
                 <|> (R2C . I2R <$> atType v IntegerType)
      _ -> atType v ty

mkPair :: TypedValue -> TypedValue -> SourceRange -> TypedValue
mkPair lhs rhs sr = TypedValue $ \ty -> case ty of
  PairType t1 t2 -> PairV ty <$> atType lhs t1 <*> atType rhs t2
  _ -> typeError sr ty "a tuple" "pair"

mkITE :: TypedValue -> TypedValue -> TypedValue -> SourceRange -> TypedValue
mkITE cond yes no _ = TypedValue $ \ty ->
  ITE ty <$> atType cond BooleanType <*> atType yes ty <*> atType no ty

mkOr, mkAnd :: TypedValue -> TypedValue -> SourceRange -> TypedValue
mkOr lhs rhs sr = TypedValue $ \case
  BooleanType -> Or <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> typeError sr ty "a disjunction" "boolean"

mkAnd lhs rhs sr = TypedValue $ \case
  BooleanType -> And <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> typeError sr ty "a conjunction" "boolean"

mkNot :: TypedValue -> SourceRange -> TypedValue
mkNot arg sr = TypedValue $ \case
  BooleanType -> Not <$> atType arg BooleanType
  ty -> typeError sr ty "logical negation" "boolean"

mkEql, mkNEq, mkLT, mkLTE, mkGT, mkGTE :: TypedValue -> TypedValue -> SourceRange -> TypedValue

mkEql lhs rhs sr = TypedValue $ \case
  BooleanType -> asum
    [ Eql RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , Eql ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , Eql IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , Eql BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> typeError sr ty "the arguments to =" "some equatable"

mkNEq lhs rhs sr = TypedValue $ \case
  BooleanType -> asum
    [ NEq RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , NEq ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , NEq IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , NEq BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> typeError sr ty "the arguments to ≠" "some equatable"

mkLT lhs rhs sr = TypedValue $ \case
  BooleanType -> asum
    [ LTF <$> atType lhs RealType    <*> atType rhs RealType
    , LTI <$> atType lhs IntegerType <*> atType rhs IntegerType
    ]
  ty -> typeError sr ty "<" "some ordered"

mkGT lhs rhs sr = TypedValue $ \case
  BooleanType -> asum
    [ LTF <$> atType rhs RealType    <*> atType lhs RealType
    , LTI <$> atType rhs IntegerType <*> atType lhs IntegerType
    ]
  ty -> typeError sr ty ">" "some ordered"

mkGTE lhs rhs sr = TypedValue $ \case
  BooleanType -> asum
    [ Not <$> (LTF <$> atType lhs RealType    <*> atType rhs RealType)
    , Not <$> (LTI <$> atType lhs IntegerType <*> atType rhs IntegerType)
    ]
  ty -> typeError sr ty "≥" "some ordered"

mkLTE lhs rhs sr =  TypedValue $ \case
  BooleanType -> asum
    [ Not <$> (LTF <$> atType rhs RealType    <*> atType lhs RealType)
    , Not <$> (LTI <$> atType rhs IntegerType <*> atType lhs IntegerType)
    ]
  ty -> typeError sr ty "≤" "some ordered"

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
        -> SourceRange
        -> TypedValue
mkArith op makeI makeF makeC lhs rhs sr = TypedValue $ \ty -> case ty of
  IntegerType -> makeI <$> atType lhs IntegerType <*> atType rhs IntegerType
  RealType    -> (I2R <$> (makeI <$> atType lhs IntegerType <*> atType rhs IntegerType))
             <|> (makeF <$> atType lhs RealType <*> atType rhs RealType)
  ComplexType -> (R2C . I2R <$> (makeI <$> atType lhs IntegerType <*> atType rhs IntegerType))
             <|> (R2C <$> (makeF <$> atType lhs RealType <*> atType rhs RealType))
             <|> (makeC <$> atType lhs ComplexType <*> atType rhs ComplexType)
  _ -> typeError sr ty op "numeric"

mkAdd, mkSub, mkMul, mkDiv, mkIDiv, mkPow :: TypedValue -> TypedValue -> SourceRange -> TypedValue

mkAdd = mkArith "+" AddI AddF AddC
mkSub = mkArith "-" SubI SubF SubC
mkMul = mkArith "*" MulI MulF MulC
mkPow = mkArith "^" PowI PowF PowC

mkDiv lhs rhs sr = TypedValue $ \ty -> case ty of
  IntegerType -> Errs $ Left ["For integer division, use the // operator."]
  RealType    -> DivF <$> atType lhs RealType <*> atType rhs RealType
  ComplexType -> (R2C <$> (DivF <$> atType lhs RealType <*> atType rhs RealType))
             <|> (DivC <$> atType lhs ComplexType <*> atType rhs ComplexType)
  _ -> typeError sr ty "/" "real or complex"

mkIDiv lhs rhs sr = TypedValue $ \ty -> case ty of
  IntegerType -> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  RealType    -> I2R <$> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  ComplexType -> R2C . I2R <$> (DivI <$> atType lhs IntegerType <*> atType rhs IntegerType)
  _ -> typeError sr ty "//" "integer"

mkNeg :: TypedValue -> SourceRange -> TypedValue
mkNeg x sr = TypedValue $ \ty -> case ty of
  IntegerType -> NegI <$> atType x IntegerType
  RealType    -> (I2R . NegI <$> atType x IntegerType)
             <|> (NegF <$> atType x RealType)
  ComplexType -> (R2C . I2R . NegI <$> atType x IntegerType)
             <|> (R2C . NegF <$> atType x RealType)
             <|> (NegC <$> atType x ComplexType)
  _ -> typeError sr ty "negation" "numeric"

mkScalar :: String -> TypeProxy ty -> HaskellType ty -> SourceRange -> TypedValue
mkScalar name ty' v sr = TypedValue $ \ty -> case sameHaskellType ty ty' of
  Just Refl -> pure (Const $ Scalar ty v)
  Nothing   -> typeError sr ty name (showType ty')

mkAbs :: TypedValue -> SourceRange -> TypedValue
mkAbs x sr = TypedValue $ \ty -> case ty of
  IntegerType -> AbsI <$> atType x IntegerType
  RealType -> (AbsF <$> atType x RealType)
          <|> (AbsC <$> atType x ComplexType)
  _ -> typeError sr ty "|·|" "real or integer"

mkVar :: String -> SourceRange -> TypedValue
mkVar n sr = TypedValue $ \ty -> case someSymbolVal n of
  SomeSymbol name -> case lookupEnv name ty (envProxy Proxy) of
    Found pf -> pure (Var name ty pf)
    WrongType (SomeType ty') -> typeError sr ty' n (showType ty)
    Absent _ -> nameError sr n

data CommonFun = CommonFun
  (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))
  (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT))

mkCommonFun :: (String, CommonFun) -> TypedValue -> SourceRange -> TypedValue
mkCommonFun (name, CommonFun makeF makeC) x sr = TypedValue $ \ty -> case ty of
  RealType    -> makeF <$> atType x RealType
  ComplexType -> (R2C . makeF <$> atType x RealType)
             <|> (makeC <$> atType x ComplexType)
  _ -> typeError sr ty name "real or complex"

mkRealFun :: (String, RealFun) -> TypedValue -> SourceRange -> TypedValue
mkRealFun (name, RealFun makeF) x sr = TypedValue $ \ty -> case ty of
  RealType -> makeF <$> atType x RealType
  _ -> typeError sr ty name "real"

mkComplexFun :: (String, ComplexFun) -> TypedValue -> SourceRange -> TypedValue
mkComplexFun (name, ComplexFun resultTy makeC) x sr = TypedValue $ \ty -> case resultTy of
  RealType -> case ty of
    RealType -> makeC <$> atType x ComplexType
    ComplexType -> R2C . makeC <$> atType x ComplexType
    _ -> typeError sr ty name "real"
  ComplexType -> case ty of
    ComplexType -> makeC <$> atType x ComplexType
    _ -> typeError sr ty name "complex"
  _ -> typeError sr ty name (showType resultTy)

mkInvert, mkDark, mkLight :: TypedValue -> SourceRange -> TypedValue
mkDark c sr = TypedValue $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType black)
               <$> atType c ColorType
  _ -> typeError sr ty "a darkening operation" "color"

mkLight c sr = TypedValue $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType white)
               <$> atType c ColorType
  _ -> typeError sr ty "a lightening operation" "color"

mkInvert c sr = TypedValue $ \ty -> case ty of
  ColorType -> InvertRGB <$> atType c ColorType
  _ -> typeError sr ty "a color inversion operation" "color"

mkBlend, mkCycle, mkRGB :: TypedValue -> TypedValue -> TypedValue -> SourceRange -> TypedValue
mkBlend t c1 c2 sr = TypedValue $ \ty -> case ty of
  ColorType -> Blend <$> atType t RealType
                     <*> atType c1 ColorType
                     <*> atType c2 ColorType
  _ -> typeError sr ty "a blend operation" "color"

mkCycle t c1 c2 sr = TypedValue $ \ty -> case ty of
  ColorType -> do
    let argName = Proxy @"#cycle-arg"

        go :: forall env. KnownEnvironment env
           => NameIsAbsent "#cycle-arg" env
           -> Errs (Value '(env, 'ColorT))
        go pf = recallIsAbsent pf $ do
          argVal <- ModF <$> atType t RealType
                         <*> pure (Const (Scalar RealType 1))
          let argVar = Var argName RealType bindingEvidence
              two = Const (Scalar RealType 2)
              expr = ITE RealType
                       (LTF argVar (Const (Scalar RealType 0.5)))
                       (MulF argVar two)
                       (SubF two (MulF argVar two))
          blend <- Blend <$> pure expr
                         <*> atType c1 ColorType
                         <*> atType c2 ColorType
          pure (LocalLet argName RealType pf argVal ColorType blend)

    case lookupEnv' argName (envProxy Proxy) of
      Found' {} -> Errs $ Left ["Internal error, #cycle-arg already defined"]
      Absent' pf -> go pf

  _ -> typeError sr ty "a cyclic blend operation" "color"

mkRGB r g b sr = TypedValue $ \ty -> case ty of
  ColorType -> RGB <$> atType r RealType
                   <*> atType g RealType
                   <*> atType b RealType
  _ -> typeError sr ty "an RGB value" "color"

mkList :: [TypedValue] -> SourceRange -> TypedValue
mkList xs sr = TypedValue $ \ty -> case ty of
  ListType ity -> List ity <$> traverse (`atType` ity) xs
  _ -> typeError sr ty "a list with items" "some list"

mkMod :: TypedValue -> TypedValue -> SourceRange -> TypedValue
mkMod x y sr = TypedValue $ \ty -> case ty of
  IntegerType -> ModI <$> atType x IntegerType <*> atType y IntegerType
  RealType    -> (I2R <$> (ModI <$> atType x IntegerType <*> atType y IntegerType)) <|>
                 (ModF <$> atType x RealType <*> atType y RealType)
  ComplexType -> (R2C . I2R <$> (ModI <$> atType x IntegerType <*> atType y IntegerType)) <|>
                 (R2C <$> (ModF <$> atType x RealType <*> atType y RealType))
  _ -> typeError sr ty "a modulo operation" "ℝ or ℤ"

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
  , ("bar", ComplexFun ComplexType ConjC)
  , ("Bar", ComplexFun ComplexType ConjC)
  ]

types :: Map String FSType
types = Map.fromList
  [ ("R", RealT), ("ℝ", RealT), ("Real", RealT)
  , ("Z", IntegerT), ("ℤ", IntegerT), ("Int", IntegerT), ("Integer", IntegerT)
  , ("C", ComplexT), ("ℂ", ComplexT), ("Complex", ComplexT)
  , ("Bool", BooleanT), ("Boolean", BooleanT)
  , ("Color", ColorT)
  ]

typeGrammar :: forall r. Grammar r (ProdSR r String FSType)
typeGrammar = mdo

  toplevel <- ruleChoice
    [ productType
    , listType
    ]

  listType <- rule
    (ListT <$> (token (Identifier "List") *> token (Identifier "of") *> listType'))

  listType' <- ruleChoice [ listType, baseType ]

  productType <- ruleChoice
    [ Pair <$> (baseType <* token (Identifier "x")) <*> productType
    , baseType
    ]

  baseType <- ruleChoice
    [ scalarType
    , (token OpenParen *> toplevel <* token CloseParen)
    ]

  scalarType <- rule $ tokenMatch $ \case
    Identifier t -> Map.lookup t types
    _ -> Nothing

  pure toplevel
