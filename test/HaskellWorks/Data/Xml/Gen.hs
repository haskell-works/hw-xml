module HaskellWorks.Data.Xml.Gen
  ( byteString
  ) where

import Data.ByteString (ByteString)
import Data.Word
import Hedgehog

import qualified Data.ByteString as BS
import qualified Hedgehog.Gen    as G

byteString :: MonadGen m => Range Int -> m Word8 -> m ByteString
byteString r g = BS.pack <$> G.list r g
