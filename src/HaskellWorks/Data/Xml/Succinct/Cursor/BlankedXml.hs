
module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  ) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Xml.Conduit.Blank

newtype BlankedXml = BlankedXml [BS.ByteString] deriving (Eq, Show)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

instance FromByteString BlankedXml where
  fromByteString bs = BlankedXml (runListConduit blankXml (chunkedBy 4064 bs))
