{-# language QuasiQuotes #-}
module Actor.Viewer.ComplexSpec (spec) where

import Test.Hspec
import Text.RawString.QQ
import qualified Data.ByteString.Char8 as BS

import Data.Maybe (isJust)
import Data.Complex (Complex(..))

import Data.Codec (deserializeYAML)
import Data.DynamicValue (getDynamic)
import Actor.Ensemble (Ensemble(..))
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ContinuationRaw(..), PrepOutputSpec(..), ComplexViewer(..), anchorFunction)

-- | A `continuation:` block as it appears under a viewer. Numeric/boolean
-- defaults are quoted because `default:` is parsed as a String (same gotcha as
-- preparation output defaults).
contYaml :: String
contYaml = [r|unknown: w
anchor: c
outputs:
  - variable: root
    type: C
    default: "0"
  - variable: converged
    type: Boolean
    default: "false"
code: |
  solve w -> w^2 - w + c
  root <- solution
  converged <- not stuck
|]

expected :: ContinuationRaw
expected = ContinuationRaw
  { crUnknown = "w"
  , crAnchor  = "c"
  , crOutputs =
      [ PrepOutputSpec "root" "C" "0"
      , PrepOutputSpec "converged" "Boolean" "false"
      ]
  , crCode = "solve w -> w^2 - w + c\nroot <- solution\nconverged <- not stuck\n"
  , crDownsample = 16  -- default (downsampling-factor omitted in the YAML below)
  }

-- | A full single-viewer project whose body reads a continuation output
-- (`converged`) and whose `continuation:` block solves for a root. (Anchor is a
-- numeric constant for now; coordinate-relative anchors like `c` are M4.)
viewerYaml :: String
viewerYaml = [r|viewer:
  title: Continuation
  size: 100x100
  z-coord: c
  initial-center: "0"
  initial-pixel-size: "1/128"
  code: |
    color <- if converged then white else grey
  continuation:
    unknown: w
    anchor: "0.1"
    outputs:
      - variable: root
        type: C
        default: "0"
      - variable: converged
        type: Boolean
        default: "false"
    code: |
      solve w -> w^2 - w + c
      root <- solution
      converged <- not stuck
|]

spec :: Spec
spec = describe "continuation: block parsing" $ do
  it "decodes unknown/anchor/outputs/code into the expected ContinuationRaw" $ do
    parsed <- deserializeYAML (BS.pack contYaml)
    parsed `shouldBe` Right expected

  it "parses a viewer with a continuation: block into a ContinuationScript" $ do
    e <- deserializeYAML (BS.pack viewerYaml) :: IO (Either String Ensemble)
    case e of
      Left err  -> expectationFailure ("ensemble parse failed: " ++ err)
      Right ens -> do
        vs <- getDynamic (ensembleViewers ens)
        case vs of
          [v] -> do
            parsedCode <- getDynamic (cvCode v)
            case parsedCode of
              Left (_, err) -> expectationFailure ("viewer code parse failed: " ++ err)
              Right (SomeViewerWithContext _ _ mcont _) ->
                isJust mcont `shouldBe` True
          _ -> expectationFailure ("expected exactly one viewer, got " ++ show (length vs))

  describe "continuation anchor" $ do
    it "treats `anchor: c` (the coordinate) as identity" $
      case anchorFunction "c" "c" of
        Left err -> expectationFailure err
        Right f  -> f (3 :+ 4) `shouldBe` (3 :+ 4)
    it "treats a numeric anchor as a constant" $
      case anchorFunction "c" "0.25" of
        Left err -> expectationFailure err
        Right f  -> f (3 :+ 4) `shouldBe` (0.25 :+ 0)
