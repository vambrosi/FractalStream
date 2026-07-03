module Language.Code.ReindexSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Code.Parser
import Language.Code.Simulator
import Language.Code.Reindex (reindexCode)
import Language.Draw

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

-- | Run `x <- x + 1; x <- x * 2` starting from x = 5 in an environment
-- containing only `x`, returning the final value of `x`.
runSmall :: Either String Double
runSmall =
  let src = "x <- x + 1\nx <- x * 2"
      env = declare @"x" RealType endOfDecls
      ctx = Bind (Proxy @"x") RealType (5 :: Double) EmptyContext
  in first (`ppFullError` src)
     $ fmap ((`evalState` (ctx, ()))
             . (\code -> simulate noDraw code >> eval (Var (Proxy @"x") RealType bindingEvidence)))
     $ parseCode env noSplices src

-- | The same script, parsed against the small environment as above, but
-- then reindexed (via 'reindexCode') into a larger environment with an
-- extra, unrelated `extra` variable spliced in before `x`, then run. Should
-- evaluate identically to 'runSmall'.
runReindexed :: Either String Double
runReindexed =
  let src      = "x <- x + 1\nx <- x * 2"
      smallEnv = declare @"x" RealType endOfDecls
      bigEnv   = declare @"extra" IntegerType $ declare @"x" RealType endOfDecls
      ctx = Bind (Proxy @"extra") IntegerType (0 :: Int64)
          $ Bind (Proxy @"x") RealType (5 :: Double) EmptyContext
  in first (`ppFullError` src)
     $ fmap (\code -> (`evalState` (ctx, ()))
                       (simulate noDraw (reindexCode bigEnv code)
                        >> eval (Var (Proxy @"x") RealType bindingEvidence)))
     $ parseCode smallEnv noSplices src

-- Throwaway check: confirms reindexCode's plumbing (used by the upcoming
-- dual-number transform) before building anything on top of it. Safe to
-- delete once the transform itself has its own tests.
spec :: Spec
spec = describe "reindexCode" $
  it "evaluates the same after being lifted into a larger environment" $
    runReindexed `shouldBe` runSmall
