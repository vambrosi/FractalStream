module Examples.SaddledropSpec (spec) where

import Test.Hspec

import Data.DynamicValue (getDynamic)
import Actor.Ensemble (Ensemble(..), parseEnsembleFromFile)
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))

-- | End-to-end check (Phase C of the saddledrop-handoff plan): the actual
-- example file parses and typechecks. This is the target this whole
-- differentiate-through-a-loop effort was built for -- `potential` is a
-- compound function containing two loops, and `critical zeta ->
-- potential(zeta, p, lam)` needs the Wirtinger derivative of that looped,
-- real-valued (non-holomorphic) function.
--
-- The file has two viewers (dynamical plane + parameter plane, see its
-- header comment) sharing one configuration panel; both must parse and
-- typecheck.
--
-- Run from `fractalstream-core/` (where `stack test` runs it), so the path
-- to the repo-root `examples/` directory is one level up.
spec :: Spec
spec = describe "examples/saddledrop-full.yaml" $
  it "parses and typechecks" $ do
    e <- parseEnsembleFromFile "../examples/saddledrop-full.yaml"
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
