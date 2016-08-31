{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Index
( XmlIndex(..)
, XmlIndexAt(..)
)
where

import           Control.Arrow
import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import qualified Data.ByteString                                            as BS
import qualified Data.List                                                  as L
import           Data.Monoid
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike
import           HaskellWorks.Data.Xml.CharLike
import           HaskellWorks.Data.Xml.Grammar
import           HaskellWorks.Data.Xml.Succinct

data XmlIndex
  = XmlIndexDocument [XmlIndex]
  | XmlIndexElement String [XmlIndex]
  | XmlIndexCData BS.ByteString
  | XmlIndexComment BS.ByteString
  | XmlIndexMeta String [XmlIndex]
  | XmlIndexAttrList [(XmlIndex, XmlIndex)]
  | XmlIndexValue BS.ByteString
  | XmlIndexAttrName BS.ByteString
  | XmlIndexAttrValue BS.ByteString
  | XmlIndexError String
  deriving (Eq, Show)

class XmlIndexAt a where
  xmlIndexAt :: a -> XmlIndex

pos :: (Select1 v, Rank1 w) => XmlCursor t v w -> Position
pos c = lastPositionOf (select1 (interests c) (rank1 (balancedParens c) (cursorRank c)))

remText :: (VectorLike v, Select1 v1, Rank1 w) => XmlCursor v v1 w -> v
remText c = vDrop (toCount (pos c)) (cursorText c)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlIndexAt (XmlCursor BS.ByteString v w) where
  xmlIndexAt :: XmlCursor BS.ByteString v w -> XmlIndex
  xmlIndexAt k = case vUncons remainder of
    Just (!c, cs) | isElementStart c          -> parseElem cs
    Just (!c, _ ) | isSpace c                 -> XmlIndexAttrList $ mapAttrsFrom (firstChild k)
    Just (!c, _ ) | isAttribute && isQuote c  -> XmlIndexAttrValue remainder
    Just _        | isAttribute               -> XmlIndexAttrName remainder
    Just _                                    -> XmlIndexValue remainder
    Nothing                                   -> XmlIndexError "End of data"
    where remainder         = remText k
          mapValuesFrom     = L.unfoldr (fmap (xmlIndexAt &&& nextSibling))
          mapAttrsFrom j    = pairwise (mapValuesFrom j) >>= asAttribute -- (pairwise >=> asAttribute) <$> mapValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asAttribute v@(XmlIndexAttrName _, XmlIndexAttrValue _) = [v]
          asAttribute _  = []

          isAttribute = case remText <$> parent k >>= vUncons of
            Just (!c, _) | isSpace c -> True
            _                        -> False

          parseElem bs =
            case ABC.parse parseXmlElement bs of
              ABC.Fail {}    -> decodeErr "Unable to parse element name" bs
              ABC.Partial _  -> decodeErr "Unexpected end of string" bs
              ABC.Done i r   -> case r of
                XmlElementTypeCData     -> XmlIndexCData i
                XmlElementTypeComment   -> XmlIndexComment i
                XmlElementTypeMeta s    -> XmlIndexMeta s    (mapValuesFrom $ firstChild k)
                XmlElementTypeElement s -> XmlIndexElement s (mapValuesFrom $ firstChild k)
                XmlElementTypeDocument  -> XmlIndexDocument  (mapValuesFrom (firstChild k) ++ mapValuesFrom (nextSibling k))


decodeErr :: String -> BS.ByteString -> XmlIndex
decodeErr reason bs =
  XmlIndexError $ reason <>": " <> show (BS.take 20 bs) <> "...'"
