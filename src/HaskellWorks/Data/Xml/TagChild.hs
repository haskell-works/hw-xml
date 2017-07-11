module HaskellWorks.Data.Xml.TagChild where

import HaskellWorks.Data.Xml.Value

data TagChild
  = TagChildAttr      String String
  | TagChildDocument  [XmlValue]
  | TagChildText      String
  | TagChildElement   String [XmlValue]
  | TagChildCData     String
  | TagChildComment   String
  | TagChildMeta      String [XmlValue]
  deriving (Eq, Show)

toTagChildren :: [XmlValue] -> [TagChild]
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

toXmlValues :: TagChild -> [XmlValue]
toXmlValues (TagChildAttr      name value       ) = [XmlAttrName name, XmlAttrValue value ]
toXmlValues (TagChildDocument  children         ) = [XmlDocument children                 ]
toXmlValues (TagChildText      text             ) = [XmlText     text                     ]
toXmlValues (TagChildElement   tagName children ) = [XmlElement  tagName  children        ]
toXmlValues (TagChildCData     text             ) = [XmlCData    text                     ]
toXmlValues (TagChildComment   text             ) = [XmlComment  text                     ]
toXmlValues (TagChildMeta      metaName children) = [XmlMeta     metaName children        ]
