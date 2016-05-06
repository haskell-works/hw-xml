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
  xmlCursorType :: k -> XmlCursorType

xmlCursorType' :: Char -> XmlCursorType
xmlCursorType' c = case c of
  '[' -> XmlCursorArray
  't' -> XmlCursorBool
  'f' -> XmlCursorBool
  '0' -> XmlCursorNumber
  '1' -> XmlCursorNumber
  '2' -> XmlCursorNumber
  '3' -> XmlCursorNumber
  '4' -> XmlCursorNumber
  '5' -> XmlCursorNumber
  '6' -> XmlCursorNumber
  '7' -> XmlCursorNumber
  '8' -> XmlCursorNumber
  '9' -> XmlCursorNumber
  '+' -> XmlCursorNumber
  '-' -> XmlCursorNumber
  'n' -> XmlCursorNull
  '{' -> XmlCursorObject
  '"' -> XmlCursorString
  _   -> error "Invalid XmlCursor cursorRank"

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
