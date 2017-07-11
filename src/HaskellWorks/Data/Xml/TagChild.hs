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
toTagChildren (XmlAttrName  name:XmlAttrValue value :xs) = TagChildAttr      name value  : toTagChildren xs
toTagChildren (XmlDocument  vs                      :xs) = TagChildDocument  vs          : toTagChildren xs
toTagChildren (XmlText      text                    :xs) = TagChildText      text        : toTagChildren xs
toTagChildren (XmlElement   tagName vs              :xs) = TagChildElement   tagName vs  : toTagChildren xs
toTagChildren (XmlCData     text                    :xs) = TagChildCData     text        : toTagChildren xs
toTagChildren (XmlComment   text                    :xs) = TagChildComment   text        : toTagChildren xs
toTagChildren (XmlMeta      metaName vs             :xs) = TagChildMeta      metaName vs : toTagChildren xs
toTagChildren (XmlAttrName  _                       :xs) =                                 toTagChildren xs
toTagChildren (XmlAttrValue _                       :xs) =                                 toTagChildren xs
toTagChildren (XmlAttrList  as                      :xs) = toTagChildren as ++             toTagChildren xs
toTagChildren (XmlError     _                       :xs) =                                 toTagChildren xs
toTagChildren []                                         = []

toRawValues :: TagChild -> [RawValue]
toRawValues (TagChildAttr      name value       ) = [XmlAttrName name, XmlAttrValue value ]
toRawValues (TagChildDocument  children         ) = [XmlDocument children                 ]
toRawValues (TagChildText      text             ) = [XmlText     text                     ]
toRawValues (TagChildElement   tagName children ) = [XmlElement  tagName  children        ]
toRawValues (TagChildCData     text             ) = [XmlCData    text                     ]
toRawValues (TagChildComment   text             ) = [XmlComment  text                     ]
toRawValues (TagChildMeta      metaName children) = [XmlMeta     metaName children        ]
