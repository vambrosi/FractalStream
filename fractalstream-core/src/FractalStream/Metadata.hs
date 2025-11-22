{-# language TemplateHaskell, ForeignFunctionInterface #-}
module FractalStream.Metadata
  ( contributors
  , gitHash
  , gitBranch
  , gitDirty
  , usingJeMalloc
  ) where

import Development.IncludeFile
import GitHash
import Foreign.C

import qualified Data.ByteString.UTF8 as UTF8

$(includeFileInSource "../CONTRIBUTORS" "contributorsFileContents")

contributors :: [String]
contributors = lines (UTF8.toString contributorsFileContents)

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

gitHash :: String
gitHash = giHash gitInfo

gitBranch :: String
gitBranch = giBranch gitInfo

gitDirty :: Bool
gitDirty = giDirty gitInfo

foreign import ccall "mallctl" jemalloc :: CSize

usingJeMalloc :: Bool
usingJeMalloc = jemalloc /= 0
