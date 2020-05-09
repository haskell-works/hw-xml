{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures              #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints  #-}

module HaskellWorks.Data.Xml.TypeSpec (spec) where

import Control.Monad
import Data.String
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.Xml.Succinct.Cursor           as C
import HaskellWorks.Data.Xml.Type
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.TreeCursor as TC

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.TypeSpec" $ do
  genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8 )) (SimpleBalancedParens (DVS.Vector Word8 )))
  genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

genSpec :: forall t u.
  ( Show              t
  , Select1           t
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u
  , IsString          (XmlCursor BS.ByteString t u)
  )
  => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("XML cursor of type " ++ t) $ do
    let forXml (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
    forXml "<elem/>" $ \cursor -> do
      it "should have correct type" . requireTest $ xmlTypeAt cursor === Just XmlTypeElement
    forXml " <elem />" $ \cursor -> do
      it "should have correct type" . requireTest $ xmlTypeAt cursor === Just XmlTypeElement
    forXml "<a foo='bar' boo='buzz'><inner data='none' /></a>" $ \cursor -> do
      it "cursor can navigate to second attribute" $ requireTest $ do
        (fc >=> fc >=> ns >=> ns >=> xmlTypeAt)  cursor  === Just XmlTypeToken
      it "cursor can navigate to first attribute of an inner element" $ requireTest $ do
        (fc >=> ns >=> fc >=> fc >=> xmlTypeAt) cursor === Just XmlTypeToken
      it "cursor can navigate to first atrribute value of an inner element" $ requireTest $ do
        (fc >=> ns >=> fc >=> fc >=> ns >=> xmlTypeAt) cursor === Just XmlTypeToken
    describe "For a single element" $ do
      let cursor =  "<a>text</a>" :: XmlCursor BS.ByteString t u
      it "can navigate down and forwards" $ requireTest $ do
        (                     xmlTypeAt) cursor === Just XmlTypeElement
        (fc               >=> xmlTypeAt) cursor === Just XmlTypeToken
        (fc >=> ns        >=> xmlTypeAt) cursor === Nothing
        (fc >=> ns >=> ns >=> xmlTypeAt) cursor === Nothing
    describe "For sample Xml" $ do
      let cursor = "<widget debug=\"on\"> \
                    \  <window name=\"main_window\"> \
                    \    <dimension>500</dimension> \
                    \    <dimension>600.01e-02</dimension> \
                    \    <dimension>    false   </dimension> \
                    \  </window> \
                    \</widget>" :: XmlCursor BS.ByteString t u
      it "can navigate down and forwards" $ requireTest $ do
        (                                                 xmlTypeAt) cursor === Just XmlTypeElement     --widget
        (fc                                           >=> xmlTypeAt) cursor === Just XmlTypeAttrList    --widget attrs
        (fc >=> ns                                    >=> xmlTypeAt) cursor === Just XmlTypeElement     --window
        (fc >=> ns >=> fc                             >=> xmlTypeAt) cursor === Just XmlTypeAttrList    --window attrs
        (fc >=> ns >=> fc >=> ns                      >=> xmlTypeAt) cursor === Just XmlTypeElement     --dimension 500
        (fc >=> ns >=> fc >=> ns >=> ns               >=> xmlTypeAt) cursor === Just XmlTypeElement     --dimension 600
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns        >=> xmlTypeAt) cursor === Just XmlTypeElement     --dimension false
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> xmlTypeAt) cursor === Just XmlTypeToken       --false
