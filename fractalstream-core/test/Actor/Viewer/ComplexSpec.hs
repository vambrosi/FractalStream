{-# language QuasiQuotes #-}
module Actor.Viewer.ComplexSpec (spec) where

import Test.Hspec
import Text.RawString.QQ
import qualified Data.ByteString.Char8 as BS

import Data.Codec (deserializeYAML)
import Data.DynamicValue (getDynamic)
import Actor.Ensemble (Ensemble(..))
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))

-- | A single-viewer project whose body uses a `solve … continuing seed`. The
-- engine hides continuation entirely behind that modifier (no `continuation:`
-- block), so this should just parse like any other viewer.
viewerYaml :: String
viewerYaml = [r|viewer:
  title: Continuing
  size: 100x100
  z-coord: c
  initial-center: "0"
  initial-pixel-size: "1/128"
  code: |
    w : C <- c
    solve w -> w^2 - w + c continuing seed
    color <- if stuck then grey else white
|]

spec :: Spec
spec = describe "continuing-seed viewer parsing" $
  it "parses a viewer whose body uses `solve … continuing seed`" $ do
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
              Right SomeViewerWithContext{} -> pure ()
          _ -> expectationFailure ("expected exactly one viewer, got " ++ show (length vs))
