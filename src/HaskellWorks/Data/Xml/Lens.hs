module HaskellWorks.Data.Xml.Lens where

import Control.Lens
import Data.Text                   (Text)
import HaskellWorks.Data.Xml.Value

isTagNamed :: Text -> Value -> Bool
isTagNamed a (XmlElement b _ _) | a == b  = True
isTagNamed _     _              = False

tagNamed :: (Applicative f, Choice p) => Text -> Optic' p f Value Value
tagNamed = filtered . isTagNamed
