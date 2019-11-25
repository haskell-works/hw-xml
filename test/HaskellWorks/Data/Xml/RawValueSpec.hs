{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.RawValueSpec (spec) where

import Control.Monad
import Data.Semigroup                                  ((<>))
import Data.String
import Data.Text                                       (Text)
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor           as C
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.TreeCursor as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

attrs :: [(Text, Text)] -> RawValue
attrs as = RawAttrList $ as >>= (\(k, v) -> [RawAttrName k, RawAttrValue v])

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.ValueSpec" $ do
  genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

rawValueVia  :: XmlIndexAt (XmlCursor BS.ByteString t u)
              => Maybe (XmlCursor BS.ByteString t u) -> RawValue
rawValueVia mk = case mk of
  Just k  -> rawValueAt (xmlIndexAt k) --either (\(DecodeError e) -> XmlError e) id (rawValueAt <$> xmlIndexAt k)
  Nothing -> RawError "No such element"

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
  => String -> XmlCursor BS.ByteString t u -> SpecWith ()
genSpec t _ = do
  describe ("XML cursor of type " <> t) $ do
    let forXml (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " <> show cursor) (f cursor)

    forXml "<a/>" $ \cursor -> do
      it "should have correct value" $ requireTest $ rawValueVia (Just cursor) === RawElement "a" []

    forXml "<a attr='value'/>" $ \cursor -> do
      it "should have correct value"    $ requireTest $ rawValueVia (Just cursor) ===
        RawElement "a" [attrs [("attr", "value")]]

    forXml "<a attr='value'><b attr='value' /></a>" $ \cursor -> do
      it "should have correct value"  $ requireTest $ rawValueVia (Just cursor) ===
        RawElement "a" [attrs [("attr", "value")],
          RawElement "b" [attrs [("attr", "value")]]]

    forXml "<a>value text</a>" $ \cursor -> do
      it "should have correct value"  $ requireTest $ rawValueVia (Just cursor) ===
        RawElement "a" [RawText "value text"]

    forXml "<!-- some comment -->" $ \cursor -> do
      it "should parse space separared comment" $ requireTest $ rawValueVia (Just cursor) ===
        RawComment " some comment "

    forXml "<!--some comment ->-->" $ \cursor -> do
      it "should parse space separared comment" $ requireTest $ rawValueVia (Just cursor) ===
        RawComment "some comment ->"

    forXml "<![CDATA[a <br/> tag]]>" $ \cursor -> do
      it "should parse cdata data" $ requireTest $ rawValueVia (Just cursor) ===
        RawCData "a <br/> tag"

    forXml "<!DOCTYPE greeting [<!ELEMENT greeting (#PCDATA)>]>" $ \cursor -> do
      it "should parse metas" $ requireTest $ rawValueVia (Just cursor) ===
        RawMeta "DOCTYPE" [RawMeta "ELEMENT" []]

    forXml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a text='value'>free</a>" $ \cursor -> do
      it "should parse xml header" $ requireTest $ rawValueVia (Just cursor) ===
        RawDocument [
          attrs [("version", "1.0"), ("encoding", "UTF-8")],
          RawElement "a" [attrs [("text", "value")],
          RawText "free"]]

      it "navigate around" $ requireTest $ do
        rawValueVia (ns cursor) === RawElement "a" [attrs [("text", "value")], RawText "free"]
        rawValueVia ((ns >=> fc) cursor) === attrs [("text", "value")]
        rawValueVia ((ns >=> fc >=> fc) cursor) === RawAttrName "text"
        rawValueVia ((ns >=> fc >=> fc >=> ns) cursor) === RawAttrValue "value"
        rawValueVia ((ns >=> fc >=> ns) cursor) === RawText "free"
