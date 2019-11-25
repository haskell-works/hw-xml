{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Xml.DecodeError where

import Control.DeepSeq
import GHC.Generics

newtype DecodeError = DecodeError String deriving (Eq, Show, Generic, NFData)
