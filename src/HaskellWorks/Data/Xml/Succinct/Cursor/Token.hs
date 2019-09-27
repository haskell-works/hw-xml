
module HaskellWorks.Data.Xml.Succinct.Cursor.Token
  ( xmlTokenAt
  ) where

import Data.ByteString                                (ByteString)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Xml.Succinct.Cursor.Internal
import HaskellWorks.Data.Xml.Token.Tokenize
import Prelude                                        hiding (drop)

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString.Internal         as BSI

xmlTokenAt :: (Rank1 w, Select1 v, TestBit w) => XmlCursor ByteString v w -> Maybe (XmlToken String Double)
xmlTokenAt k = if balancedParens k .?. lastPositionOf (cursorRank k)
  then case ABC.parse parseXmlToken (drop (toCount (xmlCursorPos k)) (cursorText k)) of
    ABC.Fail    {}  -> error "Failed to parse token in cursor"
    ABC.Partial _   -> error "Failed to parse token in cursor"
    ABC.Done    _ r -> Just r
  else Nothing
