module HaskellWorks.Data.Xml.Decode where

import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.TagInfo

class Decode a where
  decode :: RawValue -> Either DecodeError a

instance Decode RawValue where
  decode = Right
  {-# INLINE decode #-}

instance Decode TagInfo where
  decode (XmlElement tagName children) = TagInfo tagName <$> toTagData children
  decode _                             = Left (DecodeError "Not an XML element")
  {-# INLINE decode #-}

fail :: String -> Either DecodeError a
fail = Left . DecodeError
