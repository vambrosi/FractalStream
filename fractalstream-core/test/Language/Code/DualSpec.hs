{-# language QuasiQuotes #-}
module Language.Code.DualSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator
import Language.Code.Parser
import Language.Code.Simulator
import Language.Code.Dual (dValue, dualizeCode)
import Language.Value.Typecheck
  (InternalIterations, InternalStuck, InternalIterationLimit, InternalSolution)
import Language.Draw
import Language.Typecheck (TC(..))
import Language.Parser.SourceRange (SourceRange(..))

import qualified Data.Map as Map
import qualified Language.Value.Parser as P
import Text.RawString.QQ

parseValue :: EnvironmentProxy env -> TypeProxy t -> String -> Either String (Value '(env, t))
parseValue env ty i =
  withEnvironment env $ withKnownType ty $
    first (`P.ppFullError` i) (P.parseValue Map.empty i)

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

spec :: Spec
spec = do

  -- Throwaway check: confirms dValue can construct a sound reference to a
  -- tracked variable's shadow (not just the constants 0/1 that `derivative`
  -- itself needs) before the dual-number transform over looped Code is built
  -- on top of it. Safe to delete once that transform has its own tests.
  describe "dValue" $
    it "reads a tracked variable's derivative from its shadow variable" $ do
      -- a, x, dx (x's shadow) are all real; x is tracked (shadow "dx"), a
      -- is not. Tracked is now an explicit name -> shadow-name map (not a
      -- fixed naming convention), so any shadow name works as long as it's
      -- declared and matches what's passed to dValue.
      let env = declare @"dx" RealType $ declare @"x" RealType $ declare @"a" RealType $ endOfDecls
          ctx = Bind (Proxy @"dx") RealType (7 :: Double)
              $ Bind (Proxy @"x")  RealType (5 :: Double)
              $ Bind (Proxy @"a")  RealType (3 :: Double)
              $ EmptyContext

      case parseValue env RealType "a * x" of
        Left e -> expectationFailure e
        Right v -> case dValue "test" (Map.singleton "x" "dx") NoSourceRange v of
          TC (Left err) -> expectationFailure (show err)
          -- d(a*x) = a*dx + x*da = a*dx (da = 0, a untracked) = 3*7 = 21
          TC (Right dv) -> evaluate dv ctx `shouldBe` 21

  -- Throwaway check: the actual B2a milestone -- differentiating a value
  -- computed by a loop, which `solve`/`critical`'s closed-form
  -- `diffClosedForm` cannot do. x^n is computed by repeated multiplication
  -- (a `while` loop, like a compound function's body would use), and its
  -- shadow should come out to the analytic derivative n*x^(n-1) without
  -- ever writing that formula down -- it falls out of the chain rule
  -- applied once per loop iteration.
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
          -- x = 2, n = 3: x^n = 8, n*x^(n-1) = 12. x's shadow ("dx", the
          -- seed) starts at 1; result and its shadow ("dresult") start at
          -- 0 (overwritten by the script before being read). The `while`
          -- loop's own bookkeeping (iteration count/limit/stuck) is unused
          -- by this script but must be present, same as any viewer
          -- script's environment provides it.
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

  -- Throwaway check: the actual B2b milestone -- `solve z -> f(z)` where
  -- `f` is a compound function whose body is a loop, so the closed-form
  -- `solve` (which rejects a loop via diffClosedForm) can't handle it. This
  -- exercises the whole new path together: the parser's new compound-call
  -- production, tcSolveCompound's re-splicing Newton loop, and
  -- spliceCompoundDual/dualizeCode underneath it.
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

  -- Throwaway check: the second-order (critical) milestone -- `critical z ->
  -- f(z)` where `f`'s body is a loop, so this needs `dualizeCode` applied
  -- twice (once for g = F', once more over the whole first pass to get
  -- g' = F''), not just once. (t-3)^2 is computed by squaring (t-3) via a
  -- loop; its only critical point is t = 3, and Newton on a purely
  -- quadratic gradient converges in exactly one step, so this also doubles
  -- as a sanity check that g/g' come out right, not just "close enough
  -- after many iterations".
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
