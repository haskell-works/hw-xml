module HaskellWorks.Data.Xml.TagInfo
  ( TagInfo(..)
  , toTagData
  ) where

import Data.Monoid                        ((<>))
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.TagChild
import HaskellWorks.Data.Xml.TagData

import qualified Data.Map as M

data TagInfo = TagInfo
  { tagInfoName :: String
  , tagInfoData :: TagData
  } deriving (Eq, Show)

toTagData :: [RawValue] -> DecodeResult TagData
toTagData children = DecodeOk (foldl go mempty (toTagChildren children))
  where go :: TagData -> TagChild -> TagData
        go tagData (TagChildAttr name value) = tagData <> TagData (M.singleton name value) []
        go tagData child                     = tagData <> TagData M.empty                  (toRawValues child)
