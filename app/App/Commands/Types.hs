{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CountOptions(..)
  , CreateBlankedXmlOptions(..)
  , CreateBpIndexOptions(..)
  , CreateIbIndexOptions(..)
  , CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

import App.XPath.Types (XPath)
import Data.Text       (Text)
import GHC.Generics

data DemoOptions = DemoOptions deriving (Eq, Show, Generic)

data CountOptions = CountOptions
  { input  :: FilePath
  , xpath  :: XPath
  , method :: Text
  } deriving (Eq, Show, Generic)

data CreateIndexOptions = CreateIndexOptions
  { input    :: FilePath
  , ibOutput :: FilePath
  , bpOutput :: FilePath
  , method   :: Text
  } deriving (Eq, Show, Generic)

data CreateBlankedXmlOptions = CreateBlankedXmlOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)

data CreateIbIndexOptions = CreateIbIndexOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)

data CreateBpIndexOptions = CreateBpIndexOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)
