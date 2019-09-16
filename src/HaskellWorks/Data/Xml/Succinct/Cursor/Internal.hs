{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.Internal
  ( XmlCursor(..)
  , xmlCursorPos
  ) where

import Control.DeepSeq                                    (NFData (..))
import Data.ByteString.Internal                           as BSI
import Data.String
import Data.Word
import Foreign.ForeignPtr
import GHC.Generics
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits

import qualified Data.ByteString                                      as BS
import qualified Data.ByteString.Char8                                as BSC
import qualified Data.Vector.Storable                                 as DVS
import qualified HaskellWorks.Data.BalancedParens                     as BP
import qualified HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParens as CBP

data XmlCursor t v w = XmlCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show, Generic)

instance (NFData t, NFData v, NFData w) => NFData (XmlCursor t v w) where
  rnf (XmlCursor a b c d) = rnf (a, b, c, d)

instance  (FromBlankedXml (XmlInterestBits a), FromBlankedXml (CBP.XmlBalancedParens b))
          => FromByteString (XmlCursor BS.ByteString a b) where
  fromByteString bs   = XmlCursor
    { cursorText      = bs
    , interests       = getXmlInterestBits (fromBlankedXml blankedXml)
    , balancedParens  = CBP.getXmlBalancedParens (fromBlankedXml blankedXml)
    , cursorRank      = 1
    }
    where blankedXml :: BlankedXml
          blankedXml = fromByteString bs

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (BP.SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (BP.SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (BP.SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (BP.SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (BP.SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (BP.SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (XmlCursor t v u) where
  firstChild :: XmlCursor t v u -> Maybe (XmlCursor t v u)
  firstChild k = let mq = BP.firstChild (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  nextSibling :: XmlCursor t v u -> Maybe (XmlCursor t v u)
  nextSibling k = (\q -> k { cursorRank = q }) <$> BP.nextSibling (balancedParens k) (cursorRank k)

  parent :: XmlCursor t v u -> Maybe (XmlCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: XmlCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: XmlCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

xmlCursorPos :: (Rank1 w, Select1 v) => XmlCursor s v w -> Position
xmlCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k
