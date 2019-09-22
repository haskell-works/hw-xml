{-# LANGUAGE DeriveGeneric #-}

module App.XPath.Types where

import Data.Text    (Text)
import GHC.Generics

newtype XPath = XPath
  { path :: [Text]
  } deriving (Eq, Show, Generic)
