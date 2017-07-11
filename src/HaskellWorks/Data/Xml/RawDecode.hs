module HaskellWorks.Data.Xml.RawDecode where

import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.TagInfo

class RawDecode a where
  decode :: RawValue -> Either DecodeError a

instance RawDecode RawValue where
  decode = Right
  {-# INLINE decode #-}

instance RawDecode TagInfo where
  decode (RawElement tagName children) = TagInfo tagName <$> toTagData children
  decode _                             = Left (DecodeError "Not an XML element")
  {-# INLINE decode #-}

fail :: String -> Either DecodeError a
fail = Left . DecodeError
