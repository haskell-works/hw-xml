{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CountOptions(..)
  , DemoOptions(..)
  , QueryOptions(..)
  ) where

import App.XPath.Types (XPath)
import GHC.Generics

data DemoOptions = DemoOptions deriving (Eq, Show, Generic)

newtype QueryOptions = QueryOptions
  { input :: FilePath
  } deriving (Eq, Show, Generic)

data CountOptions = CountOptions
  { input :: FilePath
  , xpath :: XPath
  } deriving (Eq, Show, Generic)
