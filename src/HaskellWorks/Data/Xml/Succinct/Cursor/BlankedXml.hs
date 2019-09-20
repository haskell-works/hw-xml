
module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  , bsToBlankedXml
  ) where

import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Xml.Conduit.Blank

import qualified Data.ByteString as BS

newtype BlankedXml = BlankedXml [BS.ByteString] deriving (Eq, Show)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

bsToBlankedXml :: BS.ByteString -> BlankedXml
bsToBlankedXml bs = BlankedXml (blankXml (chunkedBy 4064 bs))
