module HaskellWorks.Data.Xml.Decode where

import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.Value

class Decode a where
  decode :: XmlValue -> Either DecodeError a

instance Decode XmlValue where
  decode = Right
  {-# INLINE decode #-}

fail :: String -> Either DecodeError a
fail = Left . DecodeError
