{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( DemoOptions(..)
  , QueryOptions(..)
  ) where

import GHC.Generics

data DemoOptions = DemoOptions deriving (Eq, Show, Generic)

newtype QueryOptions = QueryOptions
  { input :: FilePath
  } deriving (Eq, Show, Generic)
