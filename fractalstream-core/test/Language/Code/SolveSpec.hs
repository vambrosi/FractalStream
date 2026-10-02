{-# language QuasiQuotes #-}
module Language.Code.SolveSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw

import Text.RawString.QQ

-- | Run a script with complex @z@ and @c@ in scope. Returns @solution@, the
-- final @z@, @iterations@ and @stuck@.
runC :: Complex Double      -- ^ seed value of @z@
     -> Complex Double      -- ^ value of the parameter @c@
     -> Int64               -- ^ iteration limit
     -> String
     -> Either String (Complex Double, Complex Double, Int64, Bool)
runC z0 c lim input =
  let ctx = Bind (Proxy @"z") ComplexType z0
          $ Bind (Proxy @"c") ComplexType c
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalSolution) ComplexType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType lim
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\code -> simulate noDraw code >>
                  ((,,,) <$> eval (Var (Proxy @InternalSolution) ComplexType bindingEvidence)
                         <*> eval (Var (Proxy @"z") ComplexType bindingEvidence)
                         <*> eval (Var (Proxy @InternalIterations) IntegerType bindingEvidence)
                         <*> eval (Var (Proxy @InternalStuck) BooleanType bindingEvidence))))
     $ parseCode (envProxy Proxy) noSplices input

-- | Run a script with real @x@ and @a@ in scope. Returns the real part of
-- @solution@, the final @x@, @iterations@ and @stuck@.
runR :: Double              -- ^ seed value of @x@
     -> Double              -- ^ value of the parameter @a@
     -> Int64               -- ^ iteration limit
     -> String
     -> Either String (Double, Double, Int64, Bool)
runR x0 a lim input =
  let ctx = Bind (Proxy @"x") RealType x0
          $ Bind (Proxy @"a") RealType a
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalSolution) ComplexType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType lim
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\code -> simulate noDraw code >>
                  ((,,,) <$> (realPart <$> eval (Var (Proxy @InternalSolution) ComplexType bindingEvidence))
                         <*> eval (Var (Proxy @"x") RealType bindingEvidence)
                         <*> eval (Var (Proxy @InternalIterations) IntegerType bindingEvidence)
                         <*> eval (Var (Proxy @InternalStuck) BooleanType bindingEvidence))))
     $ parseCode (envProxy Proxy) noSplices input

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

-- | The root from a successful complex solve (read from @solution@).
rootC :: Either String (Complex Double, Complex Double, Int64, Bool) -> Either String (Complex Double)
rootC = fmap (\(s, _, _, _) -> s)

-- | The final value of the unknown @z@ (should equal the original seed).
seedC :: Either String (Complex Double, Complex Double, Int64, Bool) -> Either String (Complex Double)
seedC = fmap (\(_, z, _, _) -> z)

stuckOf :: Either String (a, b, Int64, Bool) -> Either String Bool
stuckOf = fmap (\(_, _, _, s) -> s)

shouldConvergeTo :: Either String (Complex Double) -> Complex Double -> Expectation
shouldConvergeTo got want = case got of
  Left e  -> expectationFailure e
  Right z -> magnitude (z - want) `shouldSatisfy` (< 1e-7)

shouldConvergeToR :: Either String (Double, Double, Int64, Bool) -> Double -> Expectation
shouldConvergeToR got want = case got of
  Left e             -> expectationFailure e
  Right (x, _, _, _) -> abs (x - want) `shouldSatisfy` (< 1e-7)

spec :: Spec
spec = do

  describe "parsing solve / preimage" $ do

    it "parses `solve z -> z^2 + c`" $
      runC (1 :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c" `shouldSatisfy` isRight

    it "parses a preimage with `of`, `within`, and a limit clause" $
      runC (1 :+ 0) (4 :+ 0) 100
        "preimage z -> z^2 of c within 0.001 up to 50 times" `shouldSatisfy` isRight

  -- Find a zero of a complex function. `stuck` if failed to converge.
  describe "solve on complex equations" $ do

    it "converges from a seed to a nearby root of z^2 + c = 0" $
      -- c = -4, roots are ±2; seed 1 lands on +2.
      rootC (runC (1 :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c")
        `shouldConvergeTo` (2 :+ 0)

    it "finds the root nearest the seed (negative branch)" $
      rootC (runC ((-1) :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c")
        `shouldConvergeTo` ((-2) :+ 0)

    it "reaches a complex root (z^2 + 1 = 0 -> ±i)" $
      rootC (runC (0.2 :+ 0.8) (1 :+ 0) 100 "solve z -> z^2 + c")
        `shouldConvergeTo` (0 :+ 1)

    it "is not stuck when it converges within the budget" $
      stuckOf (runC (1 :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c")
        `shouldBe` Right False

    it "is stuck when the iteration budget is too small" $
      stuckOf (runC (1 :+ 0) ((-4) :+ 0) 1 "solve z -> z^2 + c")
        `shouldBe` Right True

    it "leaves the unknown z unchanged (the root goes to `solution`)" $
      -- z keeps its seed value 1; the root (2) is read from `solution`.
      seedC (runC (1 :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c")
        `shouldConvergeTo` (1 :+ 0)

    it "exposes the root through the name `solution`" $
      -- Reads `solution` by name, as a viewer script does.
      seedC (runC (1 :+ 0) ((-4) :+ 0) 100 "solve z -> z^2 + c\nz <- solution")
        `shouldConvergeTo` (2 :+ 0)

    it "defaults `solution` to 0 before any solve runs" $
      -- With no solve yet, reading `solution` yields its default value, 0.
      seedC (runC (5 :+ 3) (1 :+ 0) 100 "z <- solution")
        `shouldConvergeTo` (0 :+ 0)

  -- Real unknowns, plus the `within` and limit clauses.
  describe "solve on real equations" $ do

    it "converges to a real root (x^2 - a = 0 -> sqrt a)" $
      runR 1 2 100 "solve x -> x^2 - a" `shouldConvergeToR` sqrt 2

    it "honors a looser `within` tolerance" $
      -- A looser tolerance stops Newton earlier, near sqrt 2.
      case runR 1 2 100 "solve x -> x^2 - a within 0.01" of
        Left e                 -> expectationFailure e
        Right (x, _, _, stuck) -> do
          stuck `shouldBe` False
          abs (x - sqrt 2) `shouldSatisfy` (< 1e-2)

    it "stops at the supplied iteration limit (and reports stuck)" $
      -- Two Newton steps from 1 do not reach the default 1e-10 tolerance.
      stuckOf (runR 1 2 2 "solve x -> x^2 - a up to 2 times") `shouldBe` Right True

    it "allows `solve` inside a compound function body" $ do
      let p = [r|
define g(t):
    w : R <- t
    solve w -> w^2 - a
    result <- re solution
x <- g(x)
|]
      case runR 1 2 100 p of
        Left e             -> expectationFailure e
        Right (_, x, _, _) -> abs (x - sqrt 2) `shouldSatisfy` (< 1e-7)

    it "solves through a compound function using |.| and if/then/else" $ do
      let p = [r|
define f(t):
    s : R <- |t| * t
    result <- if s > 0 then s - a else s - a
solve x -> f(x)
|]
      runR 1 2 100 p `shouldConvergeToR` sqrt 2

  -- Preimage desugars to solve of F - v.
  describe "preimage" $ do

    it "lands on a square root branch near the seed (positive)" $
      rootC (runC (1 :+ 0) (4 :+ 0) 100 "preimage z -> z^2 of c")
        `shouldConvergeTo` (2 :+ 0)

    it "lands on the other branch when seeded there (negative)" $
      rootC (runC ((-1) :+ 0) (4 :+ 0) 100 "preimage z -> z^2 of c")
        `shouldConvergeTo` ((-2) :+ 0)

  -- Works on expression functions.
  -- Otherwise, returns a clear error.
  describe "function integration and the non-closed-form errors" $ do

    it "solves an equation written with a user-defined function" $ do
      let p = [r|
define g(t):
    result <- t^2 + c
solve z -> g(z)
|]
      rootC (runC (1 :+ 0) ((-4) :+ 0) 100 p) `shouldConvergeTo` (2 :+ 0)

    -- Known limitation. (∂(conj z)/∂z = 0, so the Newton step divides by
    -- zero, `z` becomes NaN, and NaN comparisons report `stuck = False`.)
    it "silently produces NaN for a purely anti-holomorphic body (Newton divides by a zero derivative)" $
      case rootC (runC (1 :+ 1) (1 :+ 0) 100 "solve z -> conj(z) + c") of
        Left e  -> expectationFailure e
        Right z -> realPart z `shouldSatisfy` isNaN

  -- `critical` finds z where dF/dz = 0.
  describe "critical (Newton on the gradient, closed-form)" $ do

    it "parses `critical z -> (z - 3)^2`" $
      runC (0 :+ 0) (0 :+ 0) 100 "critical z -> (z - 3)^2" `shouldSatisfy` isRight

    it "converges to the critical point of (z - 3)^2 at z = 3" $
      rootC (runC (0 :+ 0) (0 :+ 0) 100 "critical z -> (z - 3)^2")
        `shouldConvergeTo` (3 :+ 0)

    it "converges to a critical point of cos(z) (a multiple of pi)" $
      -- cos'(z) = -sin(z) = 0 at z = k*pi; seeded near 0, lands on k = 0.
      case rootC (runC (0.3 :+ 0) (0 :+ 0) 100 "critical z -> cos(z)") of
        Left e  -> expectationFailure e
        Right z -> magnitude (sin z) `shouldSatisfy` (< 1e-7)

    it "is not stuck when it converges within the budget" $
      stuckOf (runC (0 :+ 0) (0 :+ 0) 100 "critical z -> (z - 3)^2")
        `shouldBe` Right False

    it "leaves the unknown z unchanged (the critical point goes to `solution`)" $
      seedC (runC (0 :+ 0) (0 :+ 0) 100 "critical z -> (z - 3)^2")
        `shouldConvergeTo` (0 :+ 0)

    it "converges to the critical point of a real function (x - 2)^2 at x = 2" $
      runR 0 0 100 "critical x -> (x - 2)^2" `shouldConvergeToR` 2

    -- Known limitation. (dF/dz = 0 identically for F = conj(z) + c, so the
    -- first convergence check passes and the seed is returned as the
    -- solution after 0 iterations.)
    it "trivially \"succeeds\" without iterating for a purely anti-holomorphic body (its gradient is identically 0)" $ do
      let result = runC (1 :+ 1) (1 :+ 0) 100 "critical z -> conj(z) + c"
      rootC result `shouldConvergeTo` (1 :+ 1)
      stuckOf result `shouldBe` Right False
