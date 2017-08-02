module HaskellWorks.Data.Xml.IndexSpec (spec) where

import Data.Either
import Data.Serialize
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length)
import Test.Hspec
import HaskellWorks.Data.Xml.Index

import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.IndexSpec" $ do
  it "Stuff" $ require $ property $ do
    version <- forAll $ G.string          (R.linear 0 64)  G.alphaNum
    ib      <- forAll $ G.storableVector  (R.linear 0 64) (G.word64 R.constantBounded)
    bp      <- forAll $ G.storableVector  (R.linear 0 64) (G.word64 R.constantBounded)
    index   <- forAll $ pure Index
      { xiVersion        = version
      , xiInterests      = BitShown ib
      , xiBalancedParens = BitShown bp
      }
    encoded <- forAll $ pure (BitShown (encode index))
    (decode (unBitShown encoded) :: Either String Index) === Right index
