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

import Data.List (isInfixOf)
import Text.RawString.QQ

-- | Run a script with a complex unknown @z@ and a complex parameter @c@ in
-- scope (plus the usual loop bookkeeping, including @solution@), returning the
-- root (read from @solution@), the final value of @z@, the step count
-- (@iterations@), and the @stuck@ flag.
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

-- | Run a script with a real unknown @x@ and a real parameter @a@ in scope,
-- returning the root (the real part of @solution@), the final value of @x@, the
-- step count, and the @stuck@ flag.
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
      -- Read `solution` by name from the script (not via Proxy), exactly as a
      -- viewer does: the script copies it into z, which we then observe.
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
      -- A looser tolerance on |F| stops Newton earlier, so x lands in the
      -- neighborhood of sqrt 2 but is coarser than the default 1e-10 path.
      case runR 1 2 100 "solve x -> x^2 - a within 0.01" of
        Left e                 -> expectationFailure e
        Right (x, _, _, stuck) -> do
          stuck `shouldBe` False
          abs (x - sqrt 2) `shouldSatisfy` (< 1e-2)

    it "stops at the supplied iteration limit (and reports stuck)" $
      -- Two Newton steps from 1 do not reach the default 1e-10 tolerance.
      stuckOf (runR 1 2 2 "solve x -> x^2 - a up to 2 times") `shouldBe` Right True

  -- Preimage desugars to solve of F - v.
  describe "preimage" $ do

    it "lands on a square root branch near the seed (positive)" $
      rootC (runC (1 :+ 0) (4 :+ 0) 100 "preimage z -> z^2 of c")
        `shouldConvergeTo` (2 :+ 0)

    it "lands on the other branch when seeded there (negative)" $
      rootC (runC ((-1) :+ 0) (4 :+ 0) 100 "preimage z -> z^2 of c")
        `shouldConvergeTo` ((-2) :+ 0)

  -- Can be applied to one-statement user-defined functions
  -- Otherwise, returns a clear error.
  describe "function integration and the non-closed-form errors" $ do

    it "solves an equation written with a user-defined function" $ do
      let p = [r|
define g(t):
    result <- t^2 + c
solve z -> g(z)
|]
      rootC (runC (1 :+ 0) ((-4) :+ 0) 100 p) `shouldConvergeTo` (2 :+ 0)

    it "rejects a non-differentiable (non-closed-form) body with a clear error" $
      case runC (1 :+ 1) (1 :+ 0) 100 "solve z -> conj(z) + c" of
        Right _ -> expectationFailure "expected a closed-form error, but solve succeeded"
        Left e  -> e `shouldSatisfy` isInfixOf "closed-form"

  -- `critical` is `solve` on the gradient: it finds z where dF/dz = 0.
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

    it "rejects a non-differentiable (non-closed-form) body with a clear error" $
      case runC (1 :+ 1) (1 :+ 0) 100 "critical z -> conj(z) + c" of
        Right _ -> expectationFailure "expected a closed-form error, but critical succeeded"
        Left e  -> e `shouldSatisfy` isInfixOf "closed-form"
