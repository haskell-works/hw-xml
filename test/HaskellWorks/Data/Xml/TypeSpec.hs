{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.TypeSpec (spec) where

import           Control.Monad
import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor                               as TC
import           HaskellWorks.Data.Xml.Succinct.Cursor                      as C
import           HaskellWorks.Data.Xml.Succinct.Index
import           HaskellWorks.Data.Xml.Type
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "Cursor for [Bool]" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "<elem />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlTypeAt cursor `shouldBe` Just XmlTypeElement
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " <elem />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlTypeAt cursor `shouldBe` Just XmlTypeElement
    it "cursor can navigate to attr list" $ do
      let cursor = "<a foo='bar' boo='buzz'/>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList
    it "cursor can navigate through attrs" $ do
      let cursor = "<a foo='bar' boo='buzz'/>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --foo
      (fc >=> fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --bar
      (fc >=> fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --boo
      (fc >=> fc >=> ns >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --buzz
      (fc >=> fc >=> ns >=> ns >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing --back off!
    it "cursor can navigate to children" $ do
      let cursor = "<a><b /><c /></a>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement --b
      (fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement --c
      (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing --back off!
    it "cursor recognises child element as an element child next to attr list" $ do
      let cursor = "<a foo='bar'><inner /></a>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList
      (fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement
      (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing -- no more!

--   genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
--   genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
--   genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
--   genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
--   genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

-- genSpec :: forall t u.
--   ( Eq                t
--   , Show              t
--   , Select1           t
--   , Eq                u
--   , Show              u
--   , Rank0             u
--   , Rank1             u
--   , BalancedParens    u
--   , TestBit           u
--   , FromForeignRegion (XmlCursor BS.ByteString t u)
--   , IsString          (XmlCursor BS.ByteString t u)
--   , JsonIndexAt       (XmlCursor BS.ByteString t u)
--   )
--   => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
-- genSpec t _ = do
--   describe ("Json cursor of type " ++ t) $ do
--     let forJson (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
--     forJson "{}" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeObject
--     forJson " {}" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeObject
--     forJson "1234" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeNumber
--     forJson "\"Hello\"" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeString
--     forJson "[]" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeArray
--     forJson "true" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeBool
--     forJson "false" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeBool
--     forJson "null" $ \cursor -> do
--       it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just JsonTypeNull
--     forJson "[null]" $ \cursor -> do
--       it "should have correct type"       $ (fc >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeNull
--     forJson "[null, {\"field\": 1}]" $ \cursor -> do
--       it "cursor can navigate to second child of array" $ do
--         (fc >=> ns >=> xmlTypeAt)  cursor  `shouldBe` Just JsonTypeObject
--       it "cursor can navigate to first child of object at second child of array" $ do
--         (fc >=> ns >=> fc >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--       it "cursor can navigate to first child of object at second child of array" $ do
--         (fc >=> ns >=> fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeNumber
--     describe "For empty json array" $ do
--       let cursor =  "[null]" :: XmlCursor BS.ByteString t u
--       it "can navigate down and forwards" $ do
--         (                     xmlTypeAt) cursor `shouldBe` Just JsonTypeArray
--         (fc               >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeNull
--         (fc >=> ns        >=> xmlTypeAt) cursor `shouldBe` Nothing
--         (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing
--     describe "For sample Json" $ do
--       let cursor =  "{ \
--                     \    \"widget\": { \
--                     \        \"debug\": \"on\", \
--                     \        \"window\": { \
--                     \            \"name\": \"main_window\", \
--                     \            \"dimensions\": [500, 600.01e-02, true, false, null] \
--                     \        } \
--                     \    } \
--                     \}" :: XmlCursor BS.ByteString t u
--       it "can navigate down and forwards" $ do
--         (                                                                      xmlTypeAt) cursor `shouldBe` Just JsonTypeObject
--         (fc                                                                >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns                                                         >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeObject
--         (fc >=> ns >=> fc                                                  >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns                                           >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns >=> ns                                    >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeObject
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeString
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Just JsonTypeArray
