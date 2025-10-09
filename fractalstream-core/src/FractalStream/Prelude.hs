module FractalStream.Prelude
  (
    -- * Re-exports
      module Data.Kind
    , module GHC.TypeLits
    , module Control.Monad
    , module Control.Applicative
    , module Control.Monad.State
    , module Control.Monad.Reader
    , module Control.Monad.Except
    , module Data.Functor
    , module Data.String
    , module Data.Complex
    , module Data.Coerce
    , module Data.Int
    , module Data.Word
    , Exp
    , Eval
    , Pure1
    , Map
    , Set
    , (:~:)(..)
    , Proxy(..)
    , isJust
    , fromMaybe
    , listToMaybe
    , intercalate
  ) where

import Data.Proxy (Proxy(..))
import Data.Kind
import Data.Coerce
import Data.Complex
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits hiding (LTI, GTI)
import Control.Monad
import Control.Applicative hiding (Const(..))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor hiding (unzip)
import Data.String
import Fcf (Exp, Eval, Pure1)

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.List (intercalate)
import Data.Int
import Data.Word
