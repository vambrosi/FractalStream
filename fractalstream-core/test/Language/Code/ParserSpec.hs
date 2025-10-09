{-# language QuasiQuotes #-}
module Language.Code.ParserSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Code.Parser
import Language.Code.Simulator
import Language.Effect
import Data.Color

import qualified Data.Map as Map
import Text.RawString.QQ

runWithX :: forall xt
          . Scalar xt
         -> String
         -> Either String (HaskellType xt)
runWithX (Scalar xt x) input = withKnownType xt $
  let env = BindingProxy (Proxy @"x") xt EmptyEnvProxy
      ctx = Bind (Proxy @"x") xt x EmptyContext
  in fmap ((`evalState` (ctx, ()))
           . (\c -> simulate NoHandler c >> eval (Var (Proxy @"x") xt bindingEvidence)))
   $ parseCode (EP NoEffs) env Map.empty input

runWithXY :: forall xt yt
           . Scalar xt
          -> Scalar yt
          -> String
          -> Either String (HaskellType xt)
runWithXY (Scalar xt x) (Scalar yt y) input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ EmptyContext
  in fmap ((`evalState` (ctx, ()))
           . (\c -> simulate NoHandler c >> eval (Var (Proxy @"x") xt bindingEvidence)))
   $ parseCode (EP NoEffs) (envProxy Proxy) Map.empty input

runWithXYC :: forall xt yt
           . Scalar xt
          -> Scalar yt
          -> String
          -> Either String Color
runWithXYC (Scalar xt x) (Scalar yt y) input =
  withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ Bind (Proxy @"color") ColorType grey
          $ EmptyContext
  in fmap ((`evalState` (ctx, ()))
           . (\c -> simulate NoHandler c >> eval (Var (Proxy @"color") ColorType bindingEvidence)))
   $ parseCode (EP NoEffs) (envProxy Proxy) Map.empty input

spec :: Spec
spec = do

  describe "when parsing code blocks" $ do

    it "can parse if/then/else blocks" $ do

      let p1 = [r|
if true then
  pass
  x <- 1 + 2
else
  x <- 3 + 4
|]
          p2 = "x <- 1 + 3"
          p3 = [r|
if y then
  x <- 1 + 3
  pass
else
  pass
|]
      runWithX  (Scalar IntegerType 7) p1 `shouldBe` Right 3
      runWithX  (Scalar IntegerType 7) p2 `shouldBe` Right 4
      runWithXY (Scalar IntegerType 7) (Scalar BooleanType True)  p3 `shouldBe` Right 4
      runWithXY (Scalar IntegerType 7) (Scalar BooleanType False) p3 `shouldBe` Right 7

    it "can bind new variables" $ do

       let p1 = [r|
x <- 5
y : Z <- x - 2
if true then
  x <- 2 * y
else
  pass
|]
           p2 = [r|
x <- 5
y : Z <- x - 2
if true then
  x <- 2 * y
|]

       runWithX (Scalar IntegerType 0) p1 `shouldBe` Right 6
       runWithX (Scalar IntegerType 0) p2 `shouldBe` Right 6

    it "can coerce variable types" $ do
        let p1 = [r|
k : Z <- 1
x <- k|]
        runWithX (Scalar RealType 0) p1 `shouldBe` Right 1.0

  describe "when parsing more complex code" $ do

    it "can parse a checkered Mandelbrot program" $ do
      let mandel = [r|
C : C <- x + y i
z : C <- 0
count : Z <- 0
while |z| < 100 and count < 100
  z <- z^2 + C
  count <- count + 1
if count = 100 then
  color <- black
else if im z > 0 then
  color <- red
else
  color <- yellow|]

          runMandel (x :+ y) = runWithXYC (Scalar RealType x) (Scalar RealType y) mandel
      runMandel 0 `shouldBe` Right black
      runMandel (1 :+ 1) `shouldBe` Right yellow
      runMandel (1 :+ (-1)) `shouldBe` Right red
