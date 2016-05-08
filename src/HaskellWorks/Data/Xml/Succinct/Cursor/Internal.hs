{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.Internal
  ( XmlCursor(..)
  ) where

import qualified Data.ByteString                                          as BS
import qualified Data.ByteString.Char8                                    as BSC
import           Data.ByteString.Internal                                 as BSI
import qualified Data.List                                                as L
import qualified Data.Map                                                 as M
import           Data.String
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           Data.Word8
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Xml.Extract
import qualified HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParens    as CBP
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import           HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
import           HaskellWorks.Data.Xml.Type
import           HaskellWorks.Data.Xml.Value
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
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

instance IsString (XmlCursor String (BitShown [Bool]) (BP.SimpleBalancedParens [Bool])) where
  fromString :: String -> XmlCursor String (BitShown [Bool]) (BP.SimpleBalancedParens [Bool])
  fromString s = XmlCursor
    { cursorText      = s
    , cursorRank      = 1
    , interests       = getXmlInterestBits (fromBlankedXml blankedXml)
    , balancedParens  = CBP.getXmlBalancedParens (fromBlankedXml blankedXml)
    }
    where blankedXml :: BlankedXml
          blankedXml = fromByteString (BSC.pack s)

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
  nextSibling k = let mq = BP.nextSibling (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  parent :: XmlCursor t v u -> Maybe (XmlCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: XmlCursor t v u -> Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: XmlCursor t v u -> Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

wIsXmlNumberDigit :: Word8 -> Bool
wIsXmlNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlTypeAt (XmlCursor BS.ByteString v w) where
  xmlTypeAtPosition p k = if bpk .?. p
    then case BS.uncons (vDrop (toCount p) (cursorText k)) of
      Just (c, _) | c == _bracketleft     -> Just XmlTypeArray
      Just (c, _) | c == _t               -> Just XmlTypeBool
      Just (c, _) | c == _n               -> Just XmlTypeNull
      Just (c, _) | wIsXmlNumberDigit c  -> Just XmlTypeNumber
      Just (c, _) | c == _braceleft       -> Just XmlTypeObject
      Just (c, _) | c == _quotedbl        -> Just XmlTypeString
      _                         -> Nothing
    else Nothing
    where bpk = balancedParens k

  xmlTypeAt k = xmlTypeAtPosition p k
    where p = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
          ik  = interests k
          bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlValueAt BS.ByteString BS.ByteString (XmlCursor BS.ByteString v w) where
  xmlValueAt :: XmlCursor BS.ByteString v w -> Maybe (XmlValue BS.ByteString BS.ByteString)
  xmlValueAt k = case extractXmlSnippet remainder of
    Just (XmlTypeArray ,  _) -> Just $ XmlArray (arrayValuesAt k)
    Just (XmlTypeBool  , bs) -> case BS.uncons bs of
      Just (c, _) | c == _t   -> Just $ XmlBool True
      Just (c, _) | c == _t   -> Just $ XmlBool False
      _                       -> Nothing
    Just (XmlTypeNull  ,  _) -> Just XmlNull
    Just (XmlTypeNumber, bs) -> Just $ XmlNumber bs
    Just (XmlTypeObject,  _) -> Just $ XmlObject (mapValuesAt k)
    Just (XmlTypeString, bs) -> Just $ XmlString bs
    Nothing                   -> Nothing
    where p = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k
          remainder = (vDrop (toCount p) (cursorText k))
          genArrayValue :: XmlCursor BS.ByteString v w -> Maybe (XmlValue ByteString ByteString, XmlCursor ByteString v w)
          genArrayValue j = (,) <$> xmlValueAt j <*> nextSibling j
          arrayValuesAt :: XmlCursor BS.ByteString v w -> [XmlValue BS.ByteString BS.ByteString]
          arrayValuesAt j = case firstChild j of
            Just c  -> L.unfoldr genArrayValue c
            Nothing -> []
          mapValuesAt :: XmlCursor BS.ByteString v w -> M.Map ByteString (XmlValue ByteString ByteString)
          mapValuesAt j = M.fromList (pairwise (arrayValuesAt j) >>= asField)
          asField :: (XmlValue ByteString ByteString, XmlValue ByteString ByteString) -> [(ByteString, XmlValue ByteString ByteString)]
          asField (a, b) = case a of
            XmlString s  -> [(s, b)]
            _             -> []
          pairwise :: [a] -> [(a, a)]
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
