module Examples.SaddledropSpec (spec) where

import Test.Hspec

import Data.DynamicValue (getDynamic)
import Actor.Ensemble (Ensemble(..), parseEnsembleFromFile)
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))

-- | End-to-end check that the saddledrop example parses and typechecks.
--
-- It is the most demanding script in `examples/`: `potential` is a compound
-- function containing two loops, and `critical z -> potential(z, p, lambda)`
-- therefore needs the Wirtinger derivative of a looped, real-valued
-- (non-holomorphic) function -- taken twice, since `critical` is Newton on the
-- gradient. Nothing else in the test suite exercises that whole path against a
-- real script rather than a synthetic fragment.
--
-- The file has two viewers (dynamical plane and parameter plane) sharing one
-- configuration panel; both must parse and typecheck.
--
-- The path is relative to `fractalstream-core/`, which is where `stack test`
-- runs, so the repo-root `examples/` directory is one level up.
spec :: Spec
spec = describe "examples/saddledrop.yaml" $
  it "parses and typechecks" $ do
    e <- parseEnsembleFromFile "../examples/saddledrop.yaml"
    case e of
      Left err  -> expectationFailure ("ensemble parse failed: " ++ err)
      Right ens -> do
        vs <- getDynamic (ensembleViewers ens)
        case vs of
          [] -> expectationFailure "expected at least one viewer, got none"
          _  -> mapM_ checkViewer vs
  where
    checkViewer v = do
      parsedCode <- getDynamic (cvCode v)
      case parsedCode of
        Left (_, err) -> expectationFailure ("viewer code parse failed: " ++ err)
        Right SomeViewerWithContext{} -> pure ()
