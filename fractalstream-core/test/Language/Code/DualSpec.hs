{-# language QuasiQuotes #-}
module Language.Code.DualSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Code.Parser
import Language.Code.Simulator
import Language.Code.Dual (dualizeCode)
import Language.Value.Typecheck
  (InternalIterations, InternalStuck, InternalIterationLimit, InternalSolution)
import Language.Draw
import Language.Typecheck (TC(..))
import Language.Parser.SourceRange (SourceRange(..))

import qualified Data.Map as Map
import Text.RawString.QQ

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

spec :: Spec
spec = do

  describe "dualizeCode" $
    it "differentiates x^n (computed by a while loop) to n*x^(n-1)" $ do
      let src = [r|
w : R <- 1
k : Z <- 0
while k < n:
    w <- w * x
    k <- k + 1
result <- w
|]
          env = declare @"dresult" RealType
              $ declare @"result"  RealType
              $ declare @"dx"      RealType
              $ declare @"x"       RealType
              $ declare @"n"       IntegerType
              $ declare @InternalIterations     IntegerType
              $ declare @InternalStuck          BooleanType
              $ declare @InternalIterationLimit IntegerType
              $ endOfDecls
          -- x = 2, n = 3: x^n = 8, n*x^(n-1) = 12. The seed dx is 1.
          ctx = Bind (Proxy @"dresult") RealType (0 :: Double)
              $ Bind (Proxy @"result")  RealType (0 :: Double)
              $ Bind (Proxy @"dx")      RealType (1 :: Double)
              $ Bind (Proxy @"x")       RealType (2 :: Double)
              $ Bind (Proxy @"n")       IntegerType (3 :: Int64)
              $ Bind (Proxy @InternalIterations)     IntegerType (0 :: Int64)
              $ Bind (Proxy @InternalStuck)          BooleanType False
              $ Bind (Proxy @InternalIterationLimit) IntegerType (100 :: Int64)
              $ EmptyContext

      case parseCode env noSplices src of
        Left e -> expectationFailure (ppFullError e src)
        Right code -> case dualizeCode NoSourceRange "test" "gen" (Map.fromList [("x", "dx"), ("result", "dresult")]) code of
          TC (Left err) -> expectationFailure (show err)
          TC (Right dcode) ->
            let (resultVal, dresultVal) =
                  evalState (simulate noDraw dcode >>
                              ((,) <$> eval (Var (Proxy @"result") RealType bindingEvidence)
                                   <*> eval (Var (Proxy @"dresult") RealType bindingEvidence)))
                            (ctx, ())
            in (resultVal, dresultVal) `shouldBe` (8, 12)

  -- `solve z -> f(z)` for a compound `f` with a loop.
  describe "tcSolveCompound (solve on a compound function)" $
    it "solves z^2 - 4 = 0, where z^2 is computed by a loop, converging to z = 2" $ do
      let src = [r|
define sqMinus4(t):
    w : C <- 1
    k : Z <- 0
    while k < 2:
        w <- w * t
        k <- k + 1
    result <- w - 4
solve z -> sqMinus4(z)
|]
          env = declare @"z" ComplexType
              $ declare @InternalIterations     IntegerType
              $ declare @InternalStuck          BooleanType
              $ declare @InternalIterationLimit IntegerType
              $ declare @InternalSolution       ComplexType
              $ endOfDecls
          ctx = Bind (Proxy @"z") ComplexType (1 :+ 0)
              $ Bind (Proxy @InternalIterations)     IntegerType (0 :: Int64)
              $ Bind (Proxy @InternalStuck)          BooleanType False
              $ Bind (Proxy @InternalIterationLimit) IntegerType (100 :: Int64)
              $ Bind (Proxy @InternalSolution)       ComplexType 0
              $ EmptyContext

      case parseCode env noSplices src of
        Left e -> expectationFailure (ppFullError e src)
        Right code ->
          let sol = evalState
                      (simulate noDraw code >> eval (Var (Proxy @InternalSolution) ComplexType bindingEvidence))
                      (ctx, ())
          in magnitude (sol - (2 :+ 0)) `shouldSatisfy` (< 1e-7)

  -- `critical z -> f(z)` for a compound `f` with a loop (a second-order
  -- derivative). f = (t-3)^2 is quadratic, so Newton lands on t = 3 in one
  -- step.
  describe "tcCriticalCompound (critical on a compound function)" $
    it "finds the critical point of (t-3)^2, where the square is computed by a loop, converging to z = 3" $ do
      let src = [r|
define sqMinus3(t):
    w : C <- 1
    k : Z <- 0
    while k < 2:
        w <- w * (t - 3)
        k <- k + 1
    result <- w
critical z -> sqMinus3(z)
|]
          env = declare @"z" ComplexType
              $ declare @InternalIterations     IntegerType
              $ declare @InternalStuck          BooleanType
              $ declare @InternalIterationLimit IntegerType
              $ declare @InternalSolution       ComplexType
              $ endOfDecls
          ctx = Bind (Proxy @"z") ComplexType (0 :+ 0)
              $ Bind (Proxy @InternalIterations)     IntegerType (0 :: Int64)
              $ Bind (Proxy @InternalStuck)          BooleanType False
              $ Bind (Proxy @InternalIterationLimit) IntegerType (100 :: Int64)
              $ Bind (Proxy @InternalSolution)       ComplexType 0
              $ EmptyContext

      case parseCode env noSplices src of
        Left e -> expectationFailure (ppFullError e src)
        Right code ->
          let sol = evalState
                      (simulate noDraw code >> eval (Var (Proxy @InternalSolution) ComplexType bindingEvidence))
                      (ctx, ())
          in magnitude (sol - (3 :+ 0)) `shouldSatisfy` (< 1e-7)
