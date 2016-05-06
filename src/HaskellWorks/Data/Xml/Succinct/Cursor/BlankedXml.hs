
module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  ) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString

newtype BlankedXml = BlankedXml [BS.ByteString] deriving (Eq, Show)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

instance FromByteString BlankedXml where
  fromByteString bs = BlankedXml (runListConduit blankJson (chunkedBy 4064 bs))
