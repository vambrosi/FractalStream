module Examples.SaddledropSpec (spec) where

import Test.Hspec

import Data.DynamicValue (getDynamic)
import Actor.Ensemble (Ensemble(..), parseEnsembleFromFile)
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))

-- | Both viewers of the saddledrop example parse and typecheck. Its
-- `critical` call takes the second Wirtinger derivative of a compound,
-- real-valued function with loops.
--
-- The path is relative to `fractalstream-core/`, where `stack test` runs.
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
