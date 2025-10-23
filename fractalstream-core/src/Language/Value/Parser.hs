{-# language RecursiveDo #-}
module Language.Value.Parser
  ( parseValue
  , parseParsedValue
  , parseValueOrList
  , parseType
  , valueGrammar
  , valueGrammarWithNoSplices
  , atType
  , typeGrammar
  , ParsedValue(..)
  , parsedValue
  , Splices
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Value
import Language.Parser
import Data.Color

import GHC.Stack

type Splices = Map String ParsedValue

parseValue :: forall env ty. (KnownEnvironment env, KnownType ty)
           => Splices
           -> String
           -> Either (Either ParseError TCError) (Value '(env, ty))
parseValue splices input = do
  tv <- first Left (parseParsedValue splices input)
  case atType tv typeProxy of TC x -> first Right x

parseParsedValue :: Map String ParsedValue
                -> String
                -> Either ParseError ParsedValue
parseParsedValue splices = parse (valueGrammar splices) . tokenize

parseType :: String -> Either ParseError SomeType
parseType = fmap (`withType` SomeType) . parse typeGrammar . tokenize

parseValueOrList :: forall env ty. (KnownEnvironment env, KnownType ty)
                 => Splices
                 -> String
                 -> Either (Either ParseError TCError) (Value '(env, ty))
parseValueOrList splices input = case typeProxy @ty of
  ListType ity -> case filter (not . (`elem` " \t\n\r")) input of
    "" -> pure (List ity [])
    _  -> parseValue splices ("[" ++ input ++ "]")
  _ -> parseValue splices input

data ParsedValue = ParsedValue SourceRange
  (forall env ty. (KnownEnvironment env, KnownType ty)
    => TypeProxy ty
    -> TC (Value '(env, ty)))

parsedValue :: (forall env ty. (KnownEnvironment env, KnownType ty) => TypeProxy ty -> TC (Value '(env, ty)))
            -> SourceRange
            -> ParsedValue
parsedValue f sr = ParsedValue sr f

-- | Try to interpret the value at the given type, inserting casts if needed.
atType :: HasCallStack => (KnownEnvironment env, KnownType ty)
       => ParsedValue
       -> TypeProxy ty
       -> TC (Value '(env, ty))
atType v@(ParsedValue _ f) ty = case ty of
  RealType    -> catchError (I2R <$> atType v IntegerType) $ \_ -> f RealType
  ComplexType -> catchError (R2C <$> atType v RealType) $ \_ -> f ComplexType
  _           -> f ty

valueGrammarWithNoSplices :: forall r. Grammar r (Prod r ParsedValue)
valueGrammarWithNoSplices = valueGrammar Map.empty

valueGrammar :: forall r
              . Splices
             -> Grammar r (Prod r ParsedValue)
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
    , withSourceRange (funAp $> \sr -> ParsedValue sr $ \_ ->
                          advise sr "To avoid ambiguity when using implicit multiplication, functions must go to the left of other values")
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

  atomOrFunAp <- ruleChoice [funAp, atom]

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
        mkRainbow <$> (token (Identifier "rainbow") *>
                       (token OpenParen *> arith <* token CloseParen)))
    , withSourceRange (
        mkRGB <$> (token (Identifier "rgb") *> (token OpenParen *> arith))
              <*> (token Comma *> arith)
              <*> (token Comma *> arith <* token CloseParen))
    ]

  typ <- typeGrammar

  let reserved = (`Set.member` reservedWords)
      reservedWords = Set.fromList
        [ "dark", "light", "invert", "blend", "cycle", "rainbow", "rgb", "mod"]
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet splices

  pure toplevel

mkCast :: ParsedValue -> FSType -> SourceRange -> ParsedValue
mkCast v tgt sr = ParsedValue sr $ \ty -> withType tgt $ \tgtTy ->
  case sameHaskellType ty tgtTy of
    Nothing -> throwError (BadConversion sr (SomeType tgtTy) (Expected $ SomeType ty))
    Just Refl -> case ty of
      RealType -> tryEach (Advice sr "Could not convert to a real number")
        [ atType v RealType
        , I2R <$> atType v IntegerType
        ]
      ComplexType -> tryEach (Advice sr "Could not convert to a complex number")
        [ atType v ComplexType
        , R2C <$> atType v RealType
        , R2C . I2R <$> atType v IntegerType
        ]
      _ -> atType v ty

mkPair :: ParsedValue -> ParsedValue -> SourceRange -> ParsedValue
mkPair lhs rhs sr = ParsedValue sr $ \ty -> case ty of
  PairType t1 t2 -> PairV ty <$> atType lhs t1 <*> atType rhs t2
  _ -> throwError (Surprise sr "an ordered pair" "of some product type" (Expected $ an ty))

mkITE :: ParsedValue -> ParsedValue -> ParsedValue -> SourceRange -> ParsedValue
mkITE cond yes no sr = ParsedValue sr $ \ty ->
  ITE ty <$> atType cond BooleanType <*> atType yes ty <*> atType no ty

mkOr, mkAnd :: ParsedValue -> ParsedValue -> SourceRange -> ParsedValue
mkOr lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> Or <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> throwError (Surprise sr "the result of a disjunction" "a truth value" (Expected $ an ty))

mkAnd lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> And <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> throwError (Surprise sr "the result of a conjunction" "a truth value" (Expected $ an ty))

mkNot :: ParsedValue -> SourceRange -> ParsedValue
mkNot arg sr = ParsedValue sr $ \case
  BooleanType -> Not <$> atType arg BooleanType
  ty -> throwError (Surprise sr "the result of a logical negation" "a truth value" (Expected $ an ty))

mkEql, mkNEq, mkLT, mkLTE, mkGT, mkGTE :: ParsedValue -> ParsedValue -> SourceRange -> ParsedValue

mkEql lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to = must be of some comparable type.")
    [ Eql RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , Eql ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , Eql IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , Eql BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> throwError (Surprise sr "the result of an equality check" "a truth value" (Expected $ an ty))

mkNEq lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to ≠ must be of some comparable type.")
    [ NEq RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , NEq ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    , NEq IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , NEq BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    ]
  ty -> throwError (Surprise sr "the result of an inequality check" "a truth value" (Expected $ an ty))

mkLT lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ LTF <$> atType lhs RealType    <*> atType rhs RealType
    , LTI <$> atType lhs IntegerType <*> atType rhs IntegerType
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

mkGT lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ LTF <$> atType rhs RealType    <*> atType lhs RealType
    , LTI <$> atType rhs IntegerType <*> atType lhs IntegerType
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

mkGTE lhs rhs sr = ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ Not <$> (LTF <$> atType lhs RealType    <*> atType rhs RealType)
    , Not <$> (LTI <$> atType lhs IntegerType <*> atType rhs IntegerType)
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

mkLTE lhs rhs sr =  ParsedValue sr $ \case
  BooleanType -> tryEach (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ Not <$> (LTF <$> atType rhs RealType    <*> atType lhs RealType)
    , Not <$> (LTI <$> atType rhs IntegerType <*> atType lhs IntegerType)
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

-- | Do an arithmetic operation, but try to keep conversions at the outermost
-- layer. For example, if we want to parse "1 + 2" at ComplexT, we want the
-- addition to happen over integers with the conversion to complex happening last.
-- To do this, we'll try to interpret "1 + 2" as a integer expression followed by
-- a conversion. If that fails, we'll try it as a real expression followed by a
-- conversion. And finally if that fails, we'll try it as a complex expression.
mkArith :: (forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT))
        -> (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))
        -> (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT))
        -> ParsedValue
        -> ParsedValue
        -> SourceRange
        -> ParsedValue
mkArith makeI makeF makeC lhs rhs sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> makeI <$> atType lhs IntegerType <*> atType rhs IntegerType
  RealType    -> makeF <$> atType lhs RealType <*> atType rhs RealType
  ComplexType -> makeC <$> atType lhs ComplexType <*> atType rhs ComplexType
  _ -> throwError (Surprise sr "the result of an arithmetic operation" "of some numeric type" (Expected $ an ty))

mkAdd, mkSub, mkMul, mkDiv, mkIDiv, mkPow :: ParsedValue -> ParsedValue -> SourceRange -> ParsedValue

mkAdd = mkArith AddI AddF AddC
mkSub = mkArith SubI SubF SubC
mkMul = mkArith MulI MulF MulC
mkPow = mkArith PowI PowF PowC

mkDiv lhs rhs sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> advise sr "For integer division, use the // operator."
  RealType    -> DivF <$> atType lhs RealType <*> atType rhs RealType
  ComplexType -> DivC <$> atType lhs ComplexType <*> atType rhs ComplexType
  _ -> throwError (Surprise sr "the result of /" "of some numeric type" (Expected $ an ty))

mkIDiv lhs rhs sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> DivI <$> atType lhs IntegerType <*> atType rhs IntegerType
  _ -> throwError (Surprise sr "the result of //" "an integer" (Expected $ an ty))

mkNeg :: ParsedValue -> SourceRange -> ParsedValue
mkNeg x sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> NegI <$> atType x IntegerType
  RealType    -> NegF <$> atType x RealType
  ComplexType -> NegC <$> atType x ComplexType
  _ -> throwError (Surprise sr "the result of negation" "of some numeric type" (Expected $ an ty))

mkScalar :: String -> TypeProxy ty -> HaskellType ty -> SourceRange -> ParsedValue
mkScalar name ty' v sr = ParsedValue sr $ \ty -> case sameHaskellType ty ty' of
  Just Refl -> pure (Const $ Scalar ty v)
  Nothing   -> throwError (Surprise sr name (an $ SomeType ty') (Expected $ an ty))

mkAbs :: ParsedValue -> SourceRange -> ParsedValue
mkAbs x sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> AbsI <$> atType x IntegerType
  RealType -> tryEach (Advice sr "The argument to |·| is not a real or complex number")
     [ AbsF <$> atType x RealType
     , AbsC <$> atType x ComplexType ]
  _ -> throwError (Surprise sr "the result of |·|" "a real number or integer" (Expected $ an ty))

mkVar :: String -> SourceRange -> ParsedValue
mkVar n sr = ParsedValue sr $ \ty -> case someSymbolVal n of
  SomeSymbol name -> case lookupEnv name ty (envProxy Proxy) of
    Found pf -> pure (Var name ty pf)
    WrongType (SomeType ty') -> throwError (Surprise sr n (an $ SomeType ty') (Expected $ an ty))
    Absent _ -> throwError (MissingName sr n)

data CommonFun = CommonFun
  (forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT))
  (forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT))

mkCommonFun :: (String, CommonFun) -> ParsedValue -> SourceRange -> ParsedValue
mkCommonFun (name, CommonFun makeF makeC) x sr = ParsedValue sr $ \ty -> case ty of
  RealType    -> makeF <$> atType x RealType
  ComplexType -> makeC <$> atType x ComplexType
  _ -> throwError (Surprise sr ("the result of " ++ name) "a real or complex number" (Expected $ an ty))

mkRealFun :: (String, RealFun) -> ParsedValue -> SourceRange -> ParsedValue
mkRealFun (name, RealFun makeF) x sr = ParsedValue sr $ \ty -> case ty of
  RealType -> makeF <$> atType x RealType
  _ -> throwError (Surprise sr ("the result of " ++ name) "a real number" (Expected $ an ty))

mkComplexFun :: (String, ComplexFun) -> ParsedValue -> SourceRange -> ParsedValue
mkComplexFun (name, ComplexFun resultTy makeC) x sr = ParsedValue sr $ \ty -> case resultTy of
  RealType -> case ty of
    RealType -> makeC <$> atType x ComplexType
    ComplexType -> R2C . makeC <$> atType x ComplexType
    _ -> throwError (Surprise sr ("the result of " ++ name) "a real number" (Expected $ an ty))
  ComplexType -> case ty of
    ComplexType -> makeC <$> atType x ComplexType
    _ -> throwError (Surprise sr ("the result of " ++ name) "a complex number" (Expected $ an ty))
  _ -> throwError (Surprise sr ("the result of " ++ name) (an $ SomeType resultTy) (Expected $ an ty))

mkInvert, mkDark, mkLight :: ParsedValue -> SourceRange -> ParsedValue
mkDark c sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType black)
               <$> atType c ColorType
  _ -> throwError (Surprise sr "the result of a darkening operation" "a color" (Expected $ an ty))

mkLight c sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType white)
               <$> atType c ColorType
  _ -> throwError (Surprise sr "the result of a lightening operation" "a color" (Expected $ an ty))

mkInvert c sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> InvertRGB <$> atType c ColorType
  _ -> throwError (Surprise sr "the result of a color inversion" "a color" (Expected $ an ty))

mkBlend, mkCycle, mkRGB ::
  ParsedValue -> ParsedValue -> ParsedValue -> SourceRange -> ParsedValue
mkBlend t c1 c2 sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> Blend <$> atType t RealType
                     <*> atType c1 ColorType
                     <*> atType c2 ColorType
  _ -> throwError (Surprise sr "the result of a blend operation" "a color" (Expected $ an ty))

mkCycle t c1 c2 sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> do
    let argName = Proxy @"#cycle-arg"

        go :: forall env. KnownEnvironment env
           => NameIsAbsent "#cycle-arg" env
           -> TC (Value '(env, 'ColorT))
        go pf = recallIsAbsent pf $ do
          argVal <- ModF <$> atType t RealType <*> pure 1
          let argVar = Var argName RealType bindingEvidence
              expr = ITE RealType (argVar `LTF` 0.5) (2 * argVar) (2 - 2 * argVar)
          blend <- Blend <$> pure expr
                         <*> atType c1 ColorType
                         <*> atType c2 ColorType
          pure (LocalLet argName RealType pf argVal ColorType blend)

    case lookupEnv' argName (envProxy Proxy) of
      Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
      Absent' pf -> go pf

  _ -> throwError (Surprise sr "the result of a cyclic blend operation" "a color" (Expected $ an ty))

-- Cycle through 8 colors over the [0,1] interval:
--   red, orange, yellow, green, cyan, blue, purple, magenta
mkRainbow :: ParsedValue -> SourceRange -> ParsedValue
mkRainbow t sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> do
    let argName = Proxy @"#rainbow-arg"

        go :: forall env. KnownEnvironment env
           => NameIsAbsent "#rainbow-arg" env
           -> TC (Value '(env, 'ColorT))
        go pf = recallIsAbsent pf $ do
          argVal <- ModF <$> atType t RealType <*> pure 1
          let argVar = Var argName RealType bindingEvidence
              ifBelow = ITE tripleType . LTF argVar
              expr = ifBelow 0.5
                       (ifBelow 0.25
                          (ifBelow 0.125 (triple 0) (triple 1))
                          (ifBelow 0.375 (triple 2) (triple 3)))
                       (ifBelow 0.75
                          (ifBelow 0.625 (triple 4) (triple 5))
                          (ifBelow 0.875 (triple 6) (triple 7)))
              wheel = map (Const . Scalar ColorType)
                          [red, orange, yellow, green, cyan, blue, purple, violet, red]
              cyan = rgbToColor (0, 128, 128)

              triple i =
                let lo = fromIntegral i * 0.125
                in PairV tripleType (8 * (argVar - Const (Scalar RealType lo)))
                         (PairV pairType (wheel !! (i + 1)) (wheel !! i))

              tripleType = PairType RealType pairType
              pairType = PairType ColorType ColorType

              blend = Blend (ProjV1 tripleType expr)
                            (ProjV1 pairType (ProjV2 tripleType expr))
                            (ProjV2 pairType (ProjV2 tripleType expr))

          pure (LocalLet argName RealType pf argVal ColorType blend)

    case lookupEnv' argName (envProxy Proxy) of
      Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
      Absent' pf -> go pf

  _ -> throwError (Surprise sr "the result of a rainbow blend operation" "a color" (Expected $ an ty))

mkRGB r g b sr = ParsedValue sr $ \ty -> case ty of
  ColorType -> RGB <$> atType r RealType
                   <*> atType g RealType
                   <*> atType b RealType
  _ -> throwError (Surprise sr "an RGB value" "a color" (Expected $ an ty))

mkList :: [ParsedValue] -> SourceRange -> ParsedValue
mkList xs sr = ParsedValue sr $ \ty -> case ty of
  ListType ity -> List ity <$> traverse (\x -> atType x ity) xs
  _ -> throwError (Surprise sr "a list literal" "some kind of list" (Expected $ an ty))

mkMod :: ParsedValue -> ParsedValue -> SourceRange -> ParsedValue
mkMod x y sr = ParsedValue sr $ \ty -> case ty of
  IntegerType -> ModI <$> atType x IntegerType <*> atType y IntegerType
  RealType    -> ModF <$> atType x RealType <*> atType y RealType
  _ -> throwError (Surprise sr "the result of a modulo operation" "an integer or real number" (Expected $ an (SomeType ty)))

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

typeGrammar :: forall r. Grammar r (Prod r FSType)
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
