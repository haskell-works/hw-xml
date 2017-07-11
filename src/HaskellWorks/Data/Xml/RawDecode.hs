module HaskellWorks.Data.Xml.RawDecode where

import HaskellWorks.Data.Xml.RawValue

class RawDecode a where
  rawDecode :: RawValue -> a

instance RawDecode RawValue where
  rawDecode = id
  {-# INLINE rawDecode #-}
