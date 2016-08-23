
module HaskellWorks.Data.Xml.Succinct.Cursor.Token
  ( xmlTokenAt
  ) where

import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import           Data.ByteString.Internal                                   as BSI
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike
import           HaskellWorks.Data.Xml.Succinct.Cursor.Internal
import           HaskellWorks.Data.Xml.Token.Tokenize

xmlTokenAt :: (Rank1 w, Select1 v, TestBit w) => XmlCursor ByteString v w -> Maybe (XmlToken String Double)
xmlTokenAt k = if balancedParens k .?. lastPositionOf (cursorRank k)
  then case ABC.parse parseXmlToken (vDrop (toCount (xmlCursorPos k)) (cursorText k)) of
    ABC.Fail    {}  -> error "Failed to parse token in cursor"
    ABC.Partial _   -> error "Failed to parse token in cursor"
    ABC.Done    _ r -> Just r
  else Nothing
