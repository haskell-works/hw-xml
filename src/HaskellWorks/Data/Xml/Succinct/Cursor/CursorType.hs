{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.CursorType
  ( HasXmlCursorType(..)
  , XmlCursorType(..)
  , xmlCursorPos
  ) where

import qualified Data.ByteString                                            as BS
import           Data.Char
import           HaskellWorks.Data.Xml.Succinct.Cursor.Internal
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data XmlCursorType
  = XmlCursorArray
  | XmlCursorBool
  | XmlCursorNull
  | XmlCursorNumber
  | XmlCursorObject
  | XmlCursorString
  deriving (Eq, Show)

class HasXmlCursorType k where
  xmlCursorType :: k -> Maybe XmlCursorType

xmlCursorType' :: Char -> Maybe XmlCursorType
xmlCursorType' c = case c of
  '[' -> Just XmlCursorArray
  't' -> Just XmlCursorBool
  'f' -> Just XmlCursorBool
  '0' -> Just XmlCursorNumber
  '1' -> Just XmlCursorNumber
  '2' -> Just XmlCursorNumber
  '3' -> Just XmlCursorNumber
  '4' -> Just XmlCursorNumber
  '5' -> Just XmlCursorNumber
  '6' -> Just XmlCursorNumber
  '7' -> Just XmlCursorNumber
  '8' -> Just XmlCursorNumber
  '9' -> Just XmlCursorNumber
  '+' -> Just XmlCursorNumber
  '-' -> Just XmlCursorNumber
  'n' -> Just XmlCursorNull
  '{' -> Just XmlCursorObject
  '"' -> Just XmlCursorString
  _   -> Nothing

xmlCursorPos :: (Rank1 w, Select1 v, VectorLike s) => XmlCursor s v w -> Position
xmlCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

xmlCursorElemAt :: (Rank1 w, Select1 v, VectorLike s) => XmlCursor s v w -> Elem s
xmlCursorElemAt k = cursorText k !!! xmlCursorPos k

instance (Rank1 i, Select1 i, Rank1 b) => HasXmlCursorType (XmlCursor String i b) where
  xmlCursorType = xmlCursorType' . xmlCursorElemAt

instance (Rank1 i, Select1 i, Rank1 b) => HasXmlCursorType (XmlCursor BS.ByteString i b) where
  xmlCursorType = xmlCursorType' . chr . fromIntegral . xmlCursorElemAt
