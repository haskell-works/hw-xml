{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( DemoOptions(..)
  ) where

import GHC.Generics

data DemoOptions = DemoOptions deriving (Eq, Show, Generic)
