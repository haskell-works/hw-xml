{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.Internal
  ( XmlCursor(..)
  ) where

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import           Data.ByteString.Internal                              as BSI
import           Data.String
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           Data.Word8
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParens
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import           HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
import           HaskellWorks.Data.Xml.Type
import           HaskellWorks.Data.Xml.Value
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens             as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike

data XmlCursor t v w = XmlCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show)

instance  (FromBlankedXml (XmlInterestBits a), FromBlankedXml (XmlBalancedParens b))
          => FromByteString (XmlCursor BS.ByteString a b) where
  fromByteString bs   = XmlCursor
    { cursorText      = bs
    , interests       = getXmlInterestBits (fromBlankedXml blankedXml)
    , balancedParens  = getXmlBalancedParens (fromBlankedXml blankedXml)
    , cursorRank      = 1
    }
    where blankedXml :: BlankedXml
          blankedXml = fromByteString bs

instance IsString (XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])) where
  fromString :: String -> XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
  fromString s = XmlCursor
    { cursorText      = s
    , cursorRank      = 1
    , interests       = getXmlInterestBits (fromBlankedXml blankedXml)
    , balancedParens  = getXmlBalancedParens (fromBlankedXml blankedXml)
    }
    where blankedXml :: BlankedXml
          blankedXml = fromByteString (BSC.pack s)

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (XmlCursor t v u) where
  firstChild :: XmlCursor t v u -> XmlCursor t v u
  firstChild k = k { cursorRank = BP.firstChild (balancedParens k) (cursorRank k) }

  nextSibling :: XmlCursor t v u -> XmlCursor t v u
  nextSibling k = k { cursorRank = BP.nextSibling (balancedParens k) (cursorRank k) }

  parent :: XmlCursor t v u -> XmlCursor t v u
  parent k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }

  depth :: XmlCursor t v u -> Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: XmlCursor t v u -> Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

wIsXmlNumberDigit :: Word8 -> Bool
wIsXmlNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

instance TestBit w => XmlTypeAt (XmlCursor BS.ByteString v w) where
  xmlTypeAtPosition p k = if balancedParens k .?. p
    then case cursorText k !!! p of
      c | c == _bracketleft     -> Just XmlTypeArray
      c | c == _t               -> Just XmlTypeBool
      c | c == _n               -> Just XmlTypeNull
      c | wIsXmlNumberDigit c  -> Just XmlTypeNumber
      c | c == _braceleft       -> Just XmlTypeObject
      c | c == _quotedbl        -> Just XmlTypeString
      _                         -> Nothing
    else Nothing

  xmlTypeAt k = xmlTypeAtPosition (lastPositionOf (cursorRank k)) k

instance TestBit w => XmlValueAt BS.ByteString BS.ByteString (XmlCursor BS.ByteString v w) where
  xmlValueAt :: XmlCursor BS.ByteString v w -> Maybe (XmlValue BS.ByteString BS.ByteString)
  xmlValueAt k = case xmlTypeAtPosition p k of
    Just XmlTypeArray  -> error "Not Implemented"
    Just XmlTypeBool   -> case cursorText k !!! p of
      c | c == _t -> Just $ XmlBool True
      c | c == _t -> Just $ XmlBool False
      _           -> Nothing
    Just XmlTypeNull   -> Just XmlNull
    Just XmlTypeNumber -> error "Not Implemented"
    Just XmlTypeObject -> error "Not Implemented"
    Just XmlTypeString -> error "Not Implemented"
    Nothing             -> Nothing
    where p = lastPositionOf (cursorRank k)
