module Actor.UI
  ( UI(..)
  ) where


import Actor.Layout
import Actor.Viewer
import Actor.Event
import Language.Environment

data UI where
  UI :: forall ensembleHandle.
      { newEnsemble :: IO ensembleHandle
      , runSetup :: ensembleHandle
                 -> String
                 -> Layout
                 -> IO ()
                 -> IO ()
      , makeLayout :: ensembleHandle
                   -> String
                   -> Layout
                   -> IO (IO ())
      , makeViewer :: ensembleHandle
                   -> IO ()
                   -> SomeContext EventArgument_
                   -> IO ()
                   -> IO ()
                   -> Viewer
                   -> IO (IO ())
      } -> UI
