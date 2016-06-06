{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Index where

import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString                                            as BS
import qualified Data.List                                                  as L
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
import           HaskellWorks.Data.Xml.Succinct

data XmlIndex
  = XmlIndexElement [XmlIndex]
  | XmlIndexComment BS.ByteString
  | XmlIndexCDataStart
  | XmlIndexMeta
  | XmlIndexAttrList [(BS.ByteString, BS.ByteString)]
  | XmlIndexString BS.ByteString
  | XmlIndexToken BS.ByteString

  deriving (Eq, Show)

class XmlIndexAt a where
  xmlIndexAt :: a -> Either DecodeError XmlIndex

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlIndexAt (XmlCursor BS.ByteString v w) where
  xmlIndexAt k = case vUncons remainder of
    Just (!c, _) | isElementStart c   -> XmlIndexElement <$> mapValuesFrom (firstChild k)
    Just (!c, _) | isQuote c          -> Right (XmlIndexString remainder)
    Just (!c, _) | isSpace c          -> XmlIndexAttrList  <$> mapAttrsFrom (firstChild k)
    Just _                            -> Right (XmlIndexToken remainder)
    Nothing                           -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = vDrop (toCount p) (cursorText k)
          mapValuesFrom j   = sequence (L.unfoldr (fmap (xmlIndexAt &&& nextSibling)) j)
          mapAttrsFrom j    = (pairwise >=> asField) <$> mapValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (XmlIndexToken a, XmlIndexString b) = [(a, b)]
          asField _  = []

        --   parseElem v = case vUncons v of
        --     Just (!c, !r) | isExclam c -> case vUncons r of
        --       Just (!c', !r') | isHyphen c'      -> Right $ XmlIndexComment ( vDrop 1 r')
        --       Just (!c', _)   | isOpenBracket c' -> Right XmlIndexCDataStart
        --       Just _                             -> Right XmlIndexMeta
        --       Nothing                            -> Left (DecodeError "Invalid Xml Meta")
        --     Just (!c, _) | isQuestion c -> Right XmlIndexMeta
