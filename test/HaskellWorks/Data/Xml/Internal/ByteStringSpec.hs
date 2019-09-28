module HaskellWorks.Data.Xml.Internal.ByteStringSpec
  ( spec
  ) where

import Data.ByteString                           (ByteString)
import Data.Char
import Data.Semigroup                            ((<>))
import Data.Word
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Xml.Internal.Blank
import HaskellWorks.Data.Xml.Internal.ByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString           as BS
import qualified HaskellWorks.Data.Xml.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Internal.ListSpec" $ do
  it "xxx" $ requireProperty $ do
    bss <- forAll $ G.list (R.linear 0 5) (G.byteString (R.linear 0 32) (G.word8 R.constantBounded))

    annotateShow $ realign 8 bss

    mconcat (fmap (uncurry (<>)) (realign 8 bss)) === padAligned 8 0 (mconcat bss)
