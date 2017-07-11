module HaskellWorks.Data.Xml.Decode where

import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.Value

class Decode a where
  decode :: Value -> Either DecodeError a

instance Decode Value where
  decode = Right
  {-# INLINE decode #-}

fail :: String -> Either DecodeError a
fail = Left . DecodeError
