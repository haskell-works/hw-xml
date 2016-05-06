{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Value where

import Data.Map

data XmlValue s n
  = XmlString s
  | XmlNumber n
  | XmlObject (Map s (XmlValue s n))
  | XmlArray [XmlValue s n]
  | XmlBool Bool
  | XmlNull
  deriving (Eq, Show)

class XmlValueAt s n a where
  xmlValueAt :: a -> Maybe (XmlValue s n)
