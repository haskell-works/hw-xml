{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Internal.ToIbBp64
  ( toBalancedParens64
  , toInterestBits64
  , toIbBp64
  ) where

import Data.ByteString                                  (ByteString)
import HaskellWorks.Data.Xml.Internal.BalancedParens
import HaskellWorks.Data.Xml.Internal.List
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml (BlankedXml (..))

toBalancedParens64 :: BlankedXml -> [ByteString]
toBalancedParens64 (BlankedXml bj) = compressWordAsBit (blankedXmlToBalancedParens bj)

toInterestBits64 :: BlankedXml -> [ByteString]
toInterestBits64 (BlankedXml bj) = blankedXmlToInterestBits bj

toIbBp64 :: BlankedXml -> [(ByteString, ByteString)]
toIbBp64 bj = zip (toInterestBits64 bj) (toBalancedParens64 bj)
