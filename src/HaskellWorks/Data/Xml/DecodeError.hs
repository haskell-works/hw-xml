{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Xml.DecodeError where

import Control.DeepSeq
import Data.Text       (Text)
import GHC.Generics

newtype DecodeError = DecodeError Text deriving (Eq, Show, Generic, NFData)
