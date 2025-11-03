{-# language OverloadedStrings #-}
module Actor.Configuration
  ( Configuration(..)
  ) where

import Actor.Layout
import Language.Code.Parser (Splices)
import Data.Codec
import Data.DynamicValue

data Configuration = Configuration
  { coTitle    :: Parsed String
  , coSize     :: Variable Dimensions
  , coContents :: Layout
  }

instance CodecWith (Dynamic (Either String Splices)) Configuration where
  codecWith_ splices = do
    title <-coTitle-< mapped (key "title") $ \_ -> pure nonEmptyString
    size  <-coSize-< key "size"
    body  <-coContents-< codecWith splices
    build Configuration title size body
