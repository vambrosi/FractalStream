
{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject)
import UI.ProjectEditor (editProject)
import UI.PendingRenders
import UI.Session
import Data.DynamicValue
import Data.Codec
import Actor.Ensemble
import Actor.Viewer (ViewerCompiler)

import qualified Data.Yaml as YAML

import UI.Welcome

import Backend
import Graphics.UI.WX --(start)
import Graphics.UI.WXCore.Draw (withFontStyle)
import Graphics.UI.WXCore.Frame (windowChildren)
import Graphics.UI.WXCore.WxcClasses ( styledTextCtrlStyleSetFont
                                     , styledTextCtrlSetText
                                     , wxcAppSetAppName )

import Control.Exception (Exception, catch, ErrorCall(..))
import qualified Data.ByteString as BS

main :: IO ()
main = withBackend $ \complexViewerCompiler -> do
  pending <- newPendingRenders
  runGUI pending complexViewerCompiler
  -- 'start' (below) returns exactly once, when the wx event loop exits --
  -- the one shutdown path guaranteed to run regardless of how the app was
  -- quit (Cmd+Q, closing the last window, etc.). Cancel any renders still
  -- in flight here, before 'withBackend' tears down the JIT session, so
  -- nothing is left calling into a kernel whose code is about to be
  -- unmapped. See UI.PendingRenders and agents/<branch>.md's "SIGSEGV on
  -- window close" note.
  drainPendingRenders pending

runGUI :: PendingRenders -> ViewerCompiler -> IO ()
runGUI pending complexViewerCompiler = start $ do

  wxcAppSetAppName "FractalStream"

  activeSessions <- newVariable []

  let projectNew = putStrLn "TODO"

      save prj f isUnsaved = do
        result <- fileSaveDialog f True True "Save FractalStream project"
                  [ ("FractalStream project files",["*.fs"]), ("All files", ["*.*"])] "" ".fs"
        case result of
          Nothing -> pure ()
          Just file -> do
            serializeYAML prj >>= BS.writeFile file
            setValue isUnsaved False

      projectOpen = \yamlFile -> do
        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow yamlFile)
              . (`catch` errorCalled projectWindow)
              . (`catch` badYaml projectWindow yamlFile)

        withRecoveryActions $ do

          prj <- either error id <$> parseEnsembleFromFile yamlFile

          sessionName <- newVariable yamlFile
          let sessionHandle = SessionHandle projectWindow
          sessionVisible <- newVariable True
          sessionUnsaved <- newVariable False
          let sessionSave = save prj projectWindow sessionUnsaved
          let si = SessionInfo{..}
          modifyValue activeSessions (si :)
          runEnsemble complexViewerCompiler
            (viewProject pending (objectCast projectWindow) (makeMenuBar ProjectActions{..}) sessionSave)
            prj

      projectOpenTemplate = \name prj -> do
        projectWindow <- frame [ visible := False ]

        sessionName <- newVariable name
        let sessionHandle = SessionHandle projectWindow
        sessionVisible <- newVariable True
        sessionUnsaved <- newVariable False
        let sessionSave = save prj projectWindow sessionUnsaved
        let si = SessionInfo{..}
        modifyValue activeSessions (si :)
        runEnsembleFromSetup complexViewerCompiler
            (viewProject pending (objectCast projectWindow) (makeMenuBar ProjectActions{..}) sessionSave)
            prj

      projectEdit = editProject

      closeSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> close f

      hideSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := False])
          mapM_ (markVisible sessionHandle False) =<< getDynamic activeSessions

      showSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := True])
          mapM_ (markVisible sessionHandle True) =<< getDynamic activeSessions

      markVisible h yn s
        | sessionHandle s == h = setValue (sessionVisible s) yn
        | otherwise = pure ()

      editSession _ = putStrLn "TODO, editSession"

  welcome ProjectActions{..}

runTemplate :: a -> b -> c -> IO ()
runTemplate _ _ _ = putStrLn "TODO: runTemplate"

badYaml :: Frame a -> FilePath -> YAML.ParseException -> IO ()
badYaml w path e = badProjectFile w path (BadProject "Bad YAML file" (show e))

badProjectFile :: Frame a -> FilePath -> BadProject -> IO ()
badProjectFile projectWindow path (BadProject why wher) =
  fatalErrorDialog projectWindow "Could not load project file" $ \p -> withFontStyle fontFixed $ \fnt -> do
    txt <- staticText p
        [ text := unlines
          [ "I was unable to load the project from " ++ path
          , "because " ++ why
          , ""
          ]]
    errTxt <- styledTextCtrl p []
    styledTextCtrlStyleSetFont errTxt 0 fnt
    styledTextCtrlSetText errTxt wher

    pure [ widget txt, expand $ widget errTxt ]

errorCalled :: Frame a -> ErrorCall -> IO ()
errorCalled projectWindow (ErrorCall msg) =
  fatalErrorDialog projectWindow "Internal error" $ \p -> withFontStyle fontFixed $ \fnt -> do
    txt <- staticText p
      [ text := unlines
        [ "Oh my."
        , ""
        , "There was an uncaught internal error:" ]]
    moreTxt <- styledTextCtrl p []
    styledTextCtrlStyleSetFont moreTxt 0 fnt
    styledTextCtrlSetText moreTxt msg
    yetMoreTxt <- staticText p
      [ text := unlines
        [ ""
        , "Perhaps you should report this at"
        , "https://github.com/matt-noonan/FractalStream/issues"
        ]
      ]
    pure [widget txt, expand $ widget moreTxt, widget yetMoreTxt]

fatalErrorDialog :: Frame a -> String -> (Panel () -> IO [Layout]) -> IO ()
fatalErrorDialog projectWindow title mkContents = do
  d <- dialog objectNull [ text := title ]
  p <- panel d []
  contents <- mkContents p
  ok <- button p [text := "OK"]
  set d [ layout := fill $ container p $ margin 15 $ floatCenter $
          column 5 (contents ++ [widget ok]) ]
  _ <- showModal d (\done -> set ok [on command := done Nothing ])
  close projectWindow

data BadProject = BadProject String String deriving Show
instance Exception BadProject
