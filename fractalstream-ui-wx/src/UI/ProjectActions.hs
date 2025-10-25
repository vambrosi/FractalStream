module UI.ProjectActions
  ( ProjectActions(..)
  , SessionHandle(..)
  , SessionInfo(..)
  ) where

import Data.DynamicValue
import Graphics.UI.WX (Frame)

newtype SessionHandle = SessionHandle (Frame ())
  deriving (Eq, Ord, Show)

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  , projectEdit :: FilePath -> IO ()
  , projectNew  :: IO ()
  , activeSessions :: SomeDynamic [SessionInfo]
  , closeSession :: SessionInfo -> IO ()
  , hideSession  :: SessionInfo -> IO ()
  , showSession  :: SessionInfo -> IO ()
  , editSession  :: SessionInfo -> IO ()
  }

data SessionInfo = SessionInfo
  { sessionName :: String
  , sessionHandle :: SessionHandle
  , sessionVisible :: Bool
  , sessionUnsaved :: Bool
  }
  deriving Show
