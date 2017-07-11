module HaskellWorks.Data.Xml.TagInfo
  ( TagInfo(..)
  ) where

import Data.Monoid                       ((<>))
import HaskellWorks.Data.Xml.Decode
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.TagChild
import HaskellWorks.Data.Xml.TagData
import HaskellWorks.Data.Xml.Value

import qualified Data.Map as M

data TagInfo = TagInfo
  { tagInfoName :: String
  , tagInfoData :: TagData
  } deriving (Eq, Show)

instance Decode TagInfo where
  decode (XmlElement tagName children) = TagInfo tagName <$> toTagData children
  decode _                             = Left (DecodeError "Not an XML element")

toTagData :: [XmlValue] -> Either DecodeError TagData
toTagData children = Right (foldl go mempty (toTagChildren children))
  where go :: TagData -> TagChild -> TagData
        go tagData (TagChildAttr name value) = tagData <> TagData (M.singleton name value) []
        go tagData child                     = tagData <> TagData M.empty                  (toXmlValues child)
