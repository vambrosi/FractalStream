{-# language OverloadedStrings #-}
module Actor.Tool
  ( Tool(..)
  , ParsedTool(..)
  , ToolInfo(..)
  , RealTool(..)
  , ComplexTool(..)
  , defaultComplexSelectionTool
  ) where

import FractalStream.Prelude

import Actor.Configuration
import Actor.Event
import Actor.Layout

import Data.Aeson

data ParsedTool = ParsedTool
  { ptoolInfo :: ToolInfo
  , ptoolDrawLayer :: Int
  , ptoolRefreshOnActivate :: Bool
  , ptoolRefreshCanUpdate :: Bool
  , ptoolConfig :: Maybe Configuration
  , ptoolEventHandlers :: ParsedEventHandlers
  }
  deriving Show

data Tool = Tool
  { toolInfo :: ToolInfo
  , toolDrawLayer :: Int
  , toolRefreshOnActivate :: Bool
  , toolConfig :: Maybe (Layout ConstantExpression)
  , toolEventHandler :: Event -> Maybe (IO ())
  , toolVars :: Set String
  }

data ToolInfo = ToolInfo
  { tiName :: String
  , tiShortcut :: Maybe Char
  , tiShortHelp :: String
  , tiHelp :: String
  }
  deriving Show

newtype RealTool = RealTool ParsedTool
  deriving Show

newtype ComplexTool = ComplexTool ParsedTool
  deriving Show

instance FromJSON (String -> String -> Either String RealTool) where
  parseJSON = withObject "tool" $ \o -> do
    tiName <- o .: "name"
    tiShortcut <- o .:? "shortcut"
    tiShortHelp <- o .:? "short-help" .!= ""
    tiHelp <- o .:? "help" .!= ""
    let ptoolInfo = ToolInfo{..}
    ptoolRefreshOnActivate <- o .:? "refresh-on-activation" .!= True
    ptoolRefreshCanUpdate <- o .:? "refresh-can-update" .!= False
    ptoolConfig <- o .:? "configuration"
    ptoolDrawLayer <- o .:? "draw-to-layer" .!= 100
    handlers <- o .:? "actions" .!= []
    pure $ \x y -> do
      let handlers' = map (($ y) . ($ x)) handlers
      ptoolEventHandlers <-
        foldl' combineEventHandlers (Right noEventHandlers) handlers'
      pure (RealTool ParsedTool{..})

instance FromJSON (String -> Either String ComplexTool) where
  parseJSON = withObject "tool" $ \o -> do
    tiName <- o .: "name"
    tiShortcut <- o .:? "shortcut"
    tiShortHelp <- o .:? "short-help" .!= ""
    tiHelp <- o .:? "help" .!= ""
    let ptoolInfo = ToolInfo{..}
    ptoolRefreshOnActivate <- o .:? "refresh-on-activation" .!= True
    ptoolRefreshCanUpdate <- o .:? "refresh-can-update" .!= False
    ptoolConfig <- o .:? "configuration"
    ptoolDrawLayer <- o .:? "draw-to-layer" .!= 100
    handlers <- o .:? "actions" .!= []
    pure $ \z -> do
      let handlers' = map (convertComplexToRealEventHandlers . ($ z)) handlers
      ptoolEventHandlers <-
        foldl' combineEventHandlers (Right noEventHandlers) handlers'
      pure (ComplexTool ParsedTool{..})

defaultComplexSelectionTool :: String -> ParsedTool
defaultComplexSelectionTool name = ParsedTool{..}
  where
    ptoolInfo = ToolInfo
      { tiName = "Select " ++ name
      , tiShortcut = Just 's'
      , tiShortHelp = ""
      , tiHelp = ""
      }
    ptoolDrawLayer = 100
    ptoolRefreshOnActivate = False
    ptoolRefreshCanUpdate = False
    ptoolConfig = Nothing
    ptoolEventHandlers = convertComplexToRealEventHandlers $ noComplexEventHandlers
      { cpehOnClick = Just (Left name, True, "pass")
      , cpehOnDrag = Just (Left name, "INTERNAL__drag_start", True, "pass")
      , cpehOnDragDone = Just (Left name, "INTERNAL__drag_start", True, "pass")
      }
