{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.Types
  ( SlowCursor
  , FastCursor
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.Xml.Succinct.Cursor

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

type SlowCursor = XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

type FastCursor = XmlCursor BS.ByteString CsPoppy1 (RangeMin2 CsPoppy1)
