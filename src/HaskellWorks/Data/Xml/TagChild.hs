module HaskellWorks.Data.Xml.TagChild where

import HaskellWorks.Data.Xml.RawValue

data TagChild
  = TagChildAttr      String String
  | TagChildDocument  [RawValue]
  | TagChildText      String
  | TagChildElement   String [RawValue]
  | TagChildCData     String
  | TagChildComment   String
  | TagChildMeta      String [RawValue]
  deriving (Eq, Show)

toTagChildren :: [RawValue] -> [TagChild]
toTagChildren (RawAttrName  name:RawAttrValue value :xs) = TagChildAttr      name value  : toTagChildren xs
toTagChildren (RawDocument  vs                      :xs) = TagChildDocument  vs          : toTagChildren xs
toTagChildren (RawText      text                    :xs) = TagChildText      text        : toTagChildren xs
toTagChildren (RawElement   tagName vs              :xs) = TagChildElement   tagName vs  : toTagChildren xs
toTagChildren (RawCData     text                    :xs) = TagChildCData     text        : toTagChildren xs
toTagChildren (RawComment   text                    :xs) = TagChildComment   text        : toTagChildren xs
toTagChildren (RawMeta      metaName vs             :xs) = TagChildMeta      metaName vs : toTagChildren xs
toTagChildren (RawAttrName  _                       :xs) =                                 toTagChildren xs
toTagChildren (RawAttrValue _                       :xs) =                                 toTagChildren xs
toTagChildren (RawAttrList  as                      :xs) = toTagChildren as ++             toTagChildren xs
toTagChildren (RawError     _                       :xs) =                                 toTagChildren xs
toTagChildren []                                         = []

toRawValues :: TagChild -> [RawValue]
toRawValues (TagChildAttr      name value       ) = [RawAttrName name, RawAttrValue value ]
toRawValues (TagChildDocument  children         ) = [RawDocument children                 ]
toRawValues (TagChildText      text             ) = [RawText     text                     ]
toRawValues (TagChildElement   tagName children ) = [RawElement  tagName  children        ]
toRawValues (TagChildCData     text             ) = [RawCData    text                     ]
toRawValues (TagChildComment   text             ) = [RawComment  text                     ]
toRawValues (TagChildMeta      metaName children) = [RawMeta     metaName children        ]
