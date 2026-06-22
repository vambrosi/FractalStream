{-# language QuasiQuotes #-}
module Language.Code.FunctionsSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw

import Text.RawString.QQ

-- | Parse and run a script (which may contain `define` blocks) in an
-- environment with a single variable @x@ (plus the usual loop bookkeeping),
-- returning the final value of @x@.
runDefine :: forall xt
           . Scalar xt
          -> String
          -> Either String (HaskellType xt)
runDefine (Scalar xt x) input = withKnownType xt $
  let env = BindingProxy (Proxy @"x") xt
          $ BindingProxy (Proxy @InternalIterations) IntegerType
          $ BindingProxy (Proxy @InternalIterationLimit) IntegerType
          $ BindingProxy (Proxy @InternalStuck) BooleanType
          $ EmptyEnvProxy
      ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"x") xt bindingEvidence)))
     $ parseCode env noSplices input

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

-- | Compare a (possibly failed) real result against an expected value, allowing
-- for floating-point round-off (derivatives of powers go through `**`).
shouldBeApprox :: Either String Double -> Double -> Expectation
shouldBeApprox got want = case got of
  Left e  -> expectationFailure e
  Right v -> abs (v - want) `shouldSatisfy` (< 1e-9)

spec :: Spec
spec = do

  -- Milestone 1: a single-argument, expression-reducible definition and one
  -- application parse and typecheck.
  describe "parsing user-defined functions" $ do

    it "parses a definition and an application" $ do
      let p = [r|
define g(t):
    result <- 2t + 1
x <- g(x)
|]
      -- Should typecheck/run without error.
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isRight

    it "accepts the function's own name as the result slot" $ do
      let p = [r|
define g(t):
    g <- 2t + 1
x <- g(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isRight

  -- Milestone 2: a call inlines and evaluates to the same value as the
  -- hand-inlined body.
  describe "inlining user-defined functions" $ do

    it "evaluates g(x) = 2x + 1 like its hand-inlined body (integers)" $ do
      let p = [r|
define g(t):
    result <- 2t + 1
x <- g(x)
|]
          handInlined v = 2 * v + 1
      runDefine (Scalar IntegerType 3)   p `shouldBe` Right (handInlined 3)
      runDefine (Scalar IntegerType 0)   p `shouldBe` Right (handInlined 0)
      runDefine (Scalar IntegerType (-5)) p `shouldBe` Right (handInlined (-5))

    it "is polymorphic: the same definition works at Real" $ do
      let p = [r|
define g(t):
    result <- 2t + 1
x <- g(x)
|]
      runDefine (Scalar RealType 2.5) p `shouldBe` Right (2 * 2.5 + 1)

    it "inlines a nested application g(g(x))" $ do
      let p = [r|
define g(t):
    result <- 2t + 1
x <- g(g(x))
|]
          g v = 2 * v + 1
      runDefine (Scalar IntegerType 3) p `shouldBe` Right (g (g 3))

  -- Full hygiene: a function's free variable resolves against its definition
  -- site, and a call made in a (let-)extended scope is re-indexed correctly.
  describe "hygienic inlining" $ do

    it "resolves a free variable against the definition site, in an extended scope" $ do
      -- h refers to the config variable x; the call happens after a local
      -- `k` has extended the environment, so the body must be re-indexed.
      let p = [r|
define h(t):
    result <- t + x
k : Z <- 10
x <- h(k)
|]
      -- x starts at 5, so h(k) = k + x = 10 + 5 = 15.
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 15

  -- Milestone 3: differentiation sees the fully-inlined (substituted) tree.
  describe "differentiating through a user function" $ do

    it "differentiates a single-argument function" $ do
      let p = [r|
define sq(t):
    result <- t^2
x <- diff(x, sq(x))
|]
      -- d/dx (x^2) = 2x; at x = 3 that is 6.
      runDefine (Scalar RealType 3) p `shouldBeApprox` 6

    it "applies the chain rule through a function" $ do
      let p = [r|
define sq(t):
    result <- t^2
x <- diff(x, sq(2x + 1))
|]
      -- d/dx (2x+1)^2 = 2(2x+1)*2 = 8x + 4; at x = 1 that is 12.
      runDefine (Scalar RealType 1) p `shouldBeApprox` 12

  -- Milestone 4: multiple parameters, multiple definitions, calls between them.
  describe "multiple parameters and definitions" $ do

    it "inlines a two-argument function" $ do
      let p = [r|
define add(a, b):
    result <- a + b
x <- add(x, 10)
|]
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 15

    it "supports several definitions and nested calls between them" $ do
      let p = [r|
define inc(t):
    result <- t + 1
define double(t):
    result <- 2t
x <- double(inc(x))
|]
      -- double(inc(5)) = double(6) = 12
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 12

    it "lets a later definition call an earlier one" $ do
      let p = [r|
define inc(t):
    result <- t + 1
define twiceInc(t):
    result <- inc(inc(t))
x <- twiceInc(x)
|]
      -- twiceInc(5) = inc(inc(5)) = 7
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 7

  -- Milestone 5a: compound (statement-bodied) functions, called in statement
  -- position (`r <- f(args)`). Bodies may use locals, reassignment, and loops.
  describe "compound function bodies (statement position)" $ do

    it "splices a body with local variables" $ do
      let p = [r|
define f(a, b):
    u : Z <- a + 1
    result <- u * b
x <- f(x, x)
|]
      -- f(5, 5): u = 6, result = 6 * 5 = 30
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 30

    it "supports a loop and reassignment in a function body" $ do
      let p = [r|
define sumTo(n):
    k : Z <- 0
    total : Z <- 0
    while k < n:
        total <- total + k
        k <- k + 1
    result <- total
x <- sumTo(x)
|]
      -- sumTo(5) = 0+1+2+3+4 = 10
      runDefine (Scalar IntegerType 5) p `shouldBe` Right 10

  -- Milestone 6: error cases. Each should produce an error (not hang/crash).
  describe "error cases" $ do

    it "rejects an arity mismatch" $ do
      let p = [r|
define g(t):
    result <- 2t + 1
x <- g(x, x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects an unknown function" $ do
      let p = "x <- nope(x)\n"
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects (rather than loops on) a recursive definition" $ do
      let p = [r|
define f(t):
    result <- f(t)
x <- f(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects a duplicate definition" $ do
      let p = [r|
define g(t):
    result <- t
define g(t):
    result <- 2t
x <- g(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects a reserved word as a function name" $ do
      let p = [r|
define cos(t):
    result <- t
x <- cos(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects a reserved word as a parameter name" $ do
      let p = [r|
define g(pi):
    result <- pi
x <- g(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft

    it "rejects a compound body that modifies an outside variable" $ do
      let p = [r|
define bad(t):
    x <- 99
    result <- t
x <- bad(x)
|]
      runDefine (Scalar IntegerType 0) p `shouldSatisfy` isLeft
