{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  , bsToBlankedXml
  , lbsToBlankedXml
  ) where

import GHC.Generics
import HaskellWorks.Data.Xml.Internal.Blank

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

newtype BlankedXml = BlankedXml
  { unblankedXml :: [BS.ByteString]
  } deriving (Eq, Show, Generic)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

bsToBlankedXml :: BS.ByteString -> BlankedXml
bsToBlankedXml bs = BlankedXml (blankXml [bs])

lbsToBlankedXml :: LBS.ByteString -> BlankedXml
lbsToBlankedXml lbs = BlankedXml (blankXml (LBS.toChunks lbs))
