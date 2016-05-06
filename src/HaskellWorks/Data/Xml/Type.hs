module HaskellWorks.Data.Xml.Type where

import HaskellWorks.Data.Positioning

data XmlType
  = XmlTypeArray
  | XmlTypeBool
  | XmlTypeNull
  | XmlTypeNumber
  | XmlTypeObject
  | XmlTypeString
  deriving (Eq, Show)

class XmlTypeAt a where
  xmlTypeAtPosition :: Position -> a -> Maybe XmlType
  xmlTypeAt :: a -> Maybe XmlType
