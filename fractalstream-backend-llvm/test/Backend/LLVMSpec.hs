{-# language QuasiQuotes #-}
module Backend.LLVMSpec (spec) where

import Test.Hspec

import Language.Value
import Data.Color
import Backend.LLVM

import Text.RawString.QQ

spec :: Spec
spec = do

  describe "when compiling programs to LLVM" $ do

    it "generates the expected code for simple programs" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ endOfDecls
      let prog1 = [r|
while x < y
  x <- x + 1
output rgb(y / x, 0, 0) to color
|]
      withCompiledCode env prog1 $ \f -> do
        f1 <- invoke' env RealType f 1.5 5
        f2 <- invoke' env RealType f 1.5 2
        f1 `shouldBe` (rgbToColor (round (255 * 5 / 5.5), 0, 0))
        f2 `shouldBe` (rgbToColor (round (255 * 2 / 2.5), 0, 0))

    it "can compile a basic Mandelbrot set program" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ declare @"maxRadius" RealType
              $ declare @"maxIter" IntegerType
              $ endOfDecls
      let mandel = [r|
C : C <- x + i y
z : C <- 0
k : Z <- 0
while |z|^2 < maxRadius^2 and k < maxIter
  z <- z^2 + C
  k <- k + 1
if k = maxIter then
  output blue to color
else
  output red to color|]

      withCompiledCode env mandel $ \f -> do
        v1 <- invoke' env ColorType f (1 :+ 1) 10 100
        v1 `shouldBe` red
        v2 <- invoke' env ColorType f ((-1) :+ 0.1) 10 100
        v2 `shouldBe` blue
        v3 <- invoke' env ColorType f ((-0.123) :+ 0.745) 10 100
        v3 `shouldBe` blue
