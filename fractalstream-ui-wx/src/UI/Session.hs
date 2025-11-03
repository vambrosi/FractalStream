module UI.Session
  ( SessionHandle(..)
  , SessionInfo(..)
  ) where

import Data.DynamicValue
import Graphics.UI.WX (Frame)

newtype SessionHandle = SessionHandle (Frame ())
  deriving (Eq, Ord, Show)

data SessionInfo = SessionInfo
  { sessionName :: Variable String
  , sessionHandle :: SessionHandle
  , sessionVisible :: Variable Bool
  , sessionUnsaved :: Variable Bool
  , sessionSave :: IO ()
  }
