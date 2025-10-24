{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import Actor.Ensemble

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject)
import UI.ProjectEditor (editProject)
import Data.DynamicValue

import Actor.Viewer.Complex (BadProject(..))

import qualified Data.Yaml as YAML

import UI.Welcome

import Backend
import Graphics.UI.WX -- (start)
import Graphics.UI.WXCore.Frame (windowChildren)

import Control.Exception (catch, ErrorCall(..))

main :: IO ()
main = withBackend $ \complexViewerCompiler -> start $ do

  sessions <- newUIValue []


  let projectNew = putStrLn "TODO"

      projectOpen = \yamlFile -> do
        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow yamlFile)
              . (`catch` errorCalled projectWindow)

        withRecoveryActions $ do
          ensemble <- YAML.decodeFileThrow yamlFile
          let si = SessionInfo
                { sessionName = yamlFile
                , sessionHandle = SessionHandle projectWindow
                , sessionVisible = True
                , sessionUnsaved = False }
          modifyUIValue sessions (si :)
          runEnsemble complexViewerCompiler
            (viewProject (objectCast projectWindow) (makeMenuBar ProjectActions{..}))
            ensemble

      projectEdit = editProject

      activeSessions = SomeDynamic sessions

      closeSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> close f

      hideSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := False])
          modifyUIValue sessions (map $ markVisible sessionHandle False)

      showSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := True])
          modifyUIValue sessions (map $ markVisible sessionHandle True)

      markVisible h yn s
        | sessionHandle s == h = s { sessionVisible = yn }
        | otherwise = s

      editSession _ = putStrLn "TODO, editSession"

  welcome ProjectActions{..}

badProjectFile :: Frame a -> FilePath -> BadProject -> IO ()
badProjectFile projectWindow path (BadProject why wher) = do

  d <- dialog objectNull [ text := "Could not load project file" ]
  p <- panel d []
  txt <- staticText p
    [ text := unlines
      [ "I was unable to load the project from " ++ path
      , "because " ++ why
      , ""
      ]]
  errTxt <- staticText p
    [ text := wher
    , font := fontFixed
    , fontSize := 14
    ]
  ok  <- button d [text := "Ok"]
  set d [ layout := fill $ container p $ margin 15 $ floatCenter $
          column 5 [ widget txt, widget errTxt, widget ok ]]
  _ <- showModal d (\done -> set ok [on command := done Nothing])
  close projectWindow

errorCalled :: Frame a -> ErrorCall -> IO ()
errorCalled projectWindow (ErrorCall msg) = do
  d <- dialog objectNull [ text := "Internal error" ]
  p <- panel d []
  txt <- staticText p
    [ text := unlines
      [ "Oh my."
      , ""
      , "There was an uncaught internal error:" ]]
  moreTxt <- staticText p [ text := msg, font := fontFixed, fontSize := 14 ]
  yetMoreTxt <- staticText p
    [ text := unlines
      [ ""
      , "Perhaps you should report this at"
      , "https://github.com/matt-noonan/FractalStream/issues"
      ]
    ]
  ok  <- button d [text := "Ok"]
  set d [ layout := fill $ container p $ margin 15 $ floatCenter $
          column 5 [ widget txt, widget moreTxt, widget yetMoreTxt, widget ok ]]
  _ <- showModal d (\done -> set ok [on command := done Nothing])
  close projectWindow
