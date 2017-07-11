module HaskellWorks.Data.Xml.RawDecode where

import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.RawValue

class RawDecode a where
  rawDecode :: RawValue -> a

instance RawDecode RawValue where
  rawDecode = id
  {-# INLINE rawDecode #-}

fail :: String -> Either DecodeError a
fail = Left . DecodeError
