{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Type where

import qualified Data.ByteString                            as BS
import           Data.Char
import           Data.Word8                                 as W8
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Drop
import qualified HaskellWorks.Data.BalancedParens           as BP
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select1
import           HaskellWorks.Data.Xml.Succinct
import           Prelude hiding (drop)

{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

data XmlType
  = XmlTypeElement
  | XmlTypeAttrList
  | XmlTypeToken
  deriving (Eq, Show)

class XmlTypeAt a where
  xmlTypeAtPosition :: Position -> a -> Maybe XmlType
  xmlTypeAt :: a -> Maybe XmlType

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlTypeAt (XmlCursor String v w) where
  xmlTypeAtPosition p k = case drop (toCount p) (cursorText k) of
    c:_ | fromIntegral (ord c) == _less      -> Just XmlTypeElement
    c:_ | W8.isSpace $ fromIntegral (ord c)  -> Just XmlTypeAttrList
    _                                        -> Just XmlTypeToken

  xmlTypeAt k = xmlTypeAtPosition p k
    where p   = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlTypeAt (XmlCursor BS.ByteString v w) where
  xmlTypeAtPosition p k = case BS.uncons (drop (toCount p) (cursorText k)) of
    Just (c, _) | c == _less     -> Just XmlTypeElement
    Just (c, _) | W8.isSpace c   -> Just XmlTypeAttrList
    _                            -> Just XmlTypeToken

  xmlTypeAt k = xmlTypeAtPosition p k
    where p   = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k
