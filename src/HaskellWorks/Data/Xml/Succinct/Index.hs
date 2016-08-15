{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Index where

import           Control.Arrow
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import qualified Data.ByteString                                            as BS
import qualified Data.List                                                  as L
import           Data.Monoid
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
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
  = XmlIndexElement String [XmlIndex]
  | XmlIndexCData BS.ByteString
  | XmlIndexComment BS.ByteString
  | XmlIndexMeta String [XmlIndex]
  | XmlIndexAttrList [(BS.ByteString, BS.ByteString)]
  | XmlIndexString BS.ByteString
  | XmlIndexValue BS.ByteString

  deriving (Eq, Show)

class XmlIndexAt a where
  xmlIndexAt :: a -> Either DecodeError XmlIndex

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlIndexAt (XmlCursor BS.ByteString v w) where
  xmlIndexAt :: XmlCursor BS.ByteString v w -> Either DecodeError XmlIndex
  xmlIndexAt k = case vUncons remainder of
    Just (!c, cs) | isElementStart c   -> parseElem cs
    Just (!c, _)  | isQuote c          -> Right (XmlIndexString remainder)
    Just (!c, _)  | isSpace c          -> XmlIndexAttrList  <$> mapAttrsFrom (firstChild k)
    Just _                             -> Right (XmlIndexValue remainder)
    Nothing                            -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = vDrop (toCount p) (cursorText k)
          mapValuesFrom j   = sequence (L.unfoldr (fmap (xmlIndexAt &&& nextSibling)) j)
          mapAttrsFrom j    = (pairwise >=> asField) <$> mapValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (XmlIndexValue a, XmlIndexString b) = [(a, b)]
          asField _  = []

          parseElem bs =
            case ABC.parse parseXmlElement bs of
              ABC.Fail {}    -> decodeErr "Unable to parse element name" bs
              ABC.Partial _  -> decodeErr "Unexpected end of string" bs
              ABC.Done i r   -> case r of
                XmlElementTypeCData     -> Right $ XmlIndexCData i
                XmlElementTypeComment   -> Right $ XmlIndexComment i
                XmlElementTypeMeta s    -> Right $ XmlIndexMeta s []
                XmlElementTypeElement s -> XmlIndexElement s <$> mapValuesFrom (firstChild k)


decodeErr :: String -> BS.ByteString -> Either DecodeError x
decodeErr reason bs =
  Left $ DecodeError (reason <>": " <> show (BS.take 20 bs) <> "...'")
