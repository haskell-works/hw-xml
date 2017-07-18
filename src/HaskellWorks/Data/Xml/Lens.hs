module HaskellWorks.Data.Xml.Lens where

import Control.Lens
import HaskellWorks.Data.Xml.Value

isTagNamed :: String -> Value -> Bool
isTagNamed a (XmlElement b _ _) | a == b  = True
isTagNamed _     _                        = False

tagNamed :: (Applicative f, Choice p) => String -> Optic' p f Value Value
tagNamed = filtered . isTagNamed
