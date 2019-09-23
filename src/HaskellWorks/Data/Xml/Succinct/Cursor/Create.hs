module HaskellWorks.Data.Xml.Succinct.Cursor.Create
  ( byteStringAsFastCursor
  , byteStringAsSlowCursor
  ) where

import Data.Coerce
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.Vector.Storable
import HaskellWorks.Data.Xml.Succinct.Cursor
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Cursor.Types

import qualified Data.ByteString                         as BS
import qualified HaskellWorks.Data.Xml.Internal.ToIbBp64 as I

byteStringAsSlowCursor :: BS.ByteString -> SlowCursor
byteStringAsSlowCursor bs = XmlCursor
  { cursorText      = bs
  , interests       = BitShown ib
  , balancedParens  = SimpleBalancedParens bp
  , cursorRank      = 1
  }
  where blankedXml = bsToBlankedXml bs
        (ib, bp) = construct64UnzipN (BS.length bs) (I.toIbBp64 blankedXml)

byteStringAsFastCursor :: BS.ByteString -> FastCursor
byteStringAsFastCursor bs = XmlCursor bs ibCsPoppy rangeMinMax r
  where XmlCursor _ ib bp r = byteStringAsSlowCursor bs
        bpCsPoppy           = makeCsPoppy (coerce bp)
        rangeMinMax         = mkRangeMin2 bpCsPoppy
        ibCsPoppy           = makeCsPoppy (coerce ib)
