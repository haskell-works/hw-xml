{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.ValueSpec (spec) where

import Control.Monad
import Data.Monoid
import Data.String
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.Xml.Succinct.Cursor           as C
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Data.Xml.Value
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.TreeCursor as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
--{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling
-- cd = TC.depth

attrs :: [(String, String)] -> XmlValue
attrs as = XmlAttrList $ as >>= (\(k, v) -> [XmlAttrName k, XmlAttrValue v])

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.ValueSpec" $ do
  genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

xmlValueVia  :: XmlIndexAt (XmlCursor BS.ByteString t u)
              => Maybe (XmlCursor BS.ByteString t u) -> XmlValue
xmlValueVia mk = case mk of
  Just k  -> xmlValueAt (xmlIndexAt k) --either (\(DecodeError e) -> XmlError e) id (xmlValueAt <$> xmlIndexAt k)
  Nothing -> XmlError "No such element"

genSpec :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u
  , FromForeignRegion (XmlCursor BS.ByteString t u)
  , IsString          (XmlCursor BS.ByteString t u)
  , XmlIndexAt        (XmlCursor BS.ByteString t u)
  )
  => String -> XmlCursor BS.ByteString t u -> SpecWith ()
genSpec t _ = do
  describe ("Json cursor of type " <> t) $ do
    let forXml (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " <> show cursor) (f cursor)

    forXml "<a/>" $ \cursor -> do
      it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` XmlElement "a" []

    forXml "<a attr='value'/>" $ \cursor -> do
      it "should have correct value"    $ xmlValueVia (Just cursor) `shouldBe`
        XmlElement "a" [attrs [("attr", "value")]]

    forXml "<a attr='value'><b attr='value' /></a>" $ \cursor -> do
      it "should have correct value"    $ xmlValueVia (Just cursor) `shouldBe`
        XmlElement "a" [attrs [("attr", "value")],
          XmlElement "b" [attrs [("attr", "value")]]]

    forXml "<a>value text</a>" $ \cursor -> do
      it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe`
        XmlElement "a" [XmlText "value text"]

    forXml "<!-- some comment -->" $ \cursor -> do
      it "should parse space separared comment" $ xmlValueVia (Just cursor) `shouldBe`
        XmlComment " some comment "

    forXml "<!--some comment ->-->" $ \cursor -> do
      it "should parse space separared comment" $ xmlValueVia (Just cursor) `shouldBe`
        XmlComment "some comment ->"

    forXml "<![CDATA[a <br/> tag]]>" $ \cursor -> do
      it "should parse cdata data" $ xmlValueVia (Just cursor) `shouldBe`
        XmlCData "a <br/> tag"

    forXml "<!DOCTYPE greeting [<!ELEMENT greeting (#PCDATA)>]>" $ \cursor -> do
      it "should parse metas" $ xmlValueVia (Just cursor) `shouldBe`
        XmlMeta "DOCTYPE" [XmlMeta "ELEMENT" []]

    forXml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a text='value'>free</a>" $ \cursor -> do
      it "should parse xml header" $ xmlValueVia (Just cursor) `shouldBe`
        XmlDocument [
          attrs [("version", "1.0"), ("encoding", "UTF-8")],
          XmlElement "a" [attrs [("text", "value")],
          XmlText "free"]]

      it "navigate around" $ do
        xmlValueVia (ns cursor) `shouldBe` XmlElement "a" [attrs [("text", "value")], XmlText "free"]
        xmlValueVia ((ns >=> fc) cursor) `shouldBe` attrs [("text", "value")]
        xmlValueVia ((ns >=> fc >=> fc) cursor) `shouldBe` XmlAttrName "text"
        xmlValueVia ((ns >=> fc >=> fc >=> ns) cursor) `shouldBe` XmlAttrValue "value"
        xmlValueVia ((ns >=> fc >=> ns) cursor) `shouldBe` XmlText "free"

    -- forXml " {}" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonObject [])
    -- forXml "1234" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonNumber 1234)
    -- forXml "\"Hello\"" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonString "Hello")
    -- forXml "[]" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    -- forXml "true" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonBool True)
    -- forXml "false" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonBool False)
    -- forXml "null" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right JsonNull
    -- forXml "[null]" $ \cursor -> do
    --   it "should have correct value"      $ xmlValueVia (Just cursor) `shouldBe` Right (JsonArray [JsonNull])
    --   it "should have correct value"      $ xmlValueVia (fc   cursor) `shouldBe` Right  JsonNull
    --   it "depth at top"                   $ cd          cursor `shouldBe` Just 1
    --   it "depth at first child of array"  $ (fc >=> cd) cursor `shouldBe` Just 2
    -- forXml "[null, {\"field\": 1}]" $ \cursor -> do
    --   it "cursor can navigate to second child of array" $ do
    --     xmlValueVia ((fc >=> ns)   cursor) `shouldBe` Right (                     JsonObject [("field", JsonNumber 1)] )
    --     xmlValueVia (Just          cursor) `shouldBe` Right (JsonArray [JsonNull, JsonObject [("field", JsonNumber 1)]])
    --   it "depth at second child of array" $ do
    --     (fc >=> ns >=> cd) cursor `shouldBe` Just 2
    --   it "depth at first child of object at second child of array" $ do
    --     (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
    --   it "depth at first child of object at second child of array" $ do
    --     (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
    -- describe "For empty json array" $ do
    --   let cursor =  "[]" :: XmlCursor BS.ByteString t u
    --   it "can navigate down and forwards" $ do
    --     xmlValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    -- describe "For empty json array" $ do
    --   let cursor =  "[null]" :: XmlCursor BS.ByteString t u
    --   it "can navigate down and forwards" $ do
    --     xmlValueVia (Just cursor) `shouldBe` Right (JsonArray [JsonNull])
    -- describe "For sample Json" $ do
    --   let cursor =  "{ \
    --                 \    \"widget\": { \
    --                 \        \"debug\": \"on\", \
    --                 \        \"window\": { \
    --                 \            \"name\": \"main_window\", \
    --                 \            \"dimensions\": [500, 600.01e-02, true, false, null] \
    --                 \        } \
    --                 \    } \
    --                 \}" :: XmlCursor BS.ByteString t u
    --   it "can navigate down and forwards" $ do
    --     let array   = JsonArray [JsonNumber 500, JsonNumber 600.01e-02, JsonBool True, JsonBool False, JsonNull] :: JsonValue
    --     let object1 = JsonObject ([("name", JsonString "main_window"), ("dimensions", array)]) :: JsonValue
    --     let object2 = JsonObject ([("debug", JsonString "on"), ("window", object1)]) :: JsonValue
    --     let object3 = JsonObject ([("widget", object2)]) :: JsonValue
    --     xmlValueVia (Just                                                                                                   cursor) `shouldBe` Right object3
    --     xmlValueVia ((fc                                                                                                  ) cursor) `shouldBe` Right (JsonString "widget"      )
    --     xmlValueVia ((fc >=> ns                                                                                           ) cursor) `shouldBe` Right (object2                  )
    --     xmlValueVia ((fc >=> ns >=> fc                                                                                    ) cursor) `shouldBe` Right (JsonString "debug"       )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns                                                                             ) cursor) `shouldBe` Right (JsonString "on"          )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns                                                                      ) cursor) `shouldBe` Right (JsonString "window"      )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns                                                               ) cursor) `shouldBe` Right (object1                  )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                                        ) cursor) `shouldBe` Right (JsonString "name"        )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                                                 ) cursor) `shouldBe` Right (JsonString "main_window" )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                                          ) cursor) `shouldBe` Right (JsonString "dimensions"  )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns                                   ) cursor) `shouldBe` Right (array                    )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                            ) cursor) `shouldBe` Right (JsonNumber 500           )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                     ) cursor) `shouldBe` Right (JsonNumber 600.01e-02    )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns              ) cursor) `shouldBe` Right (JsonBool True            )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns       ) cursor) `shouldBe` Right (JsonBool False           )
    --     xmlValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns) cursor) `shouldBe` Right JsonNull
