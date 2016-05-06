{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.Succinct.CursorSpec(spec) where

import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Xml.Succinct.Cursor                     as C
import           HaskellWorks.Data.Xml.Token
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor as TC
import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
cd = TC.depth
ss = TC.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.CursorSpec" $ do
  describe "Cursor for [Bool]" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` XmlCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType (fc cursor) `shouldBe` XmlCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType ((ns . fc) cursor) `shouldBe` XmlCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType ((fc . ns . fc) cursor) `shouldBe` XmlCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` XmlCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
  genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
  it "Loads same Xml consistentally from different backing vectors" $ do
    let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))
    let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))
    let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))
    let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
    cursorText cursor8 `shouldBe` cursorText cursor16
    cursorText cursor8 `shouldBe` cursorText cursor32
    cursorText cursor8 `shouldBe` cursorText cursor64
    let ic8   = bitShow $ interests cursor8
    let ic16  = bitShow $ interests cursor16
    let ic32  = bitShow $ interests cursor32
    let ic64  = bitShow $ interests cursor64
    ic16 `shouldBeginWith` ic8
    ic32 `shouldBeginWith` ic16
    ic64 `shouldBeginWith` ic32

shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldBeginWith as bs = take (length bs) as `shouldBe` bs

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
  , HasXmlCursorType (XmlCursor BS.ByteString t u))
  => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` XmlCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      xmlCursorType (fc cursor) `shouldBe` XmlCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      xmlCursorType ((ns . fc) cursor) `shouldBe` XmlCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      xmlCursorType ((fc . ns . fc) cursor) `shouldBe` XmlCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      xmlCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` XmlCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      xmlCursorType                                                              cursor  `shouldBe` XmlCursorObject
      xmlCursorType ((                                                       fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((                                                  ns . fc) cursor) `shouldBe` XmlCursorObject
      xmlCursorType ((                                             fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((                                        ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorObject
      xmlCursorType ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorString
      xmlCursorType ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` XmlCursorNumber
    it "can navigate up" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      (                                                        pn . fc) cursor `shouldBe`                               cursor
      (                                                   pn . ns . fc) cursor `shouldBe`                               cursor
      (                                              pn . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                                         pn . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                                    pn . ns . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                               pn . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                          pn . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (                     pn . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (                pn . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (           pn . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (      pn . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      ( pn . ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
    it "can get subtree size" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      ss                                                              cursor  `shouldBe` 45
      ss ((                                                       fc) cursor) `shouldBe` 1
      ss ((                                                  ns . fc) cursor) `shouldBe` 43
      ss ((                                             fc . ns . fc) cursor) `shouldBe` 1
      ss ((                                        ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 9
      ss ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
    it "can get token at cursor" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      xmlTokenAt                                                              cursor  `shouldBe` Just (XmlTokenBraceL                             )
      xmlTokenAt ((                                                       fc) cursor) `shouldBe` Just (XmlTokenString "widget"                    )
      xmlTokenAt ((                                                  ns . fc) cursor) `shouldBe` Just (XmlTokenBraceL                             )
      xmlTokenAt ((                                             fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "debug"                     )
      xmlTokenAt ((                                        ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "on"                        )
      xmlTokenAt ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "window"                    )
      xmlTokenAt ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenBraceL                             )
      xmlTokenAt ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "title"                     )
      xmlTokenAt ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "Sample Konfabulator Widget")
      xmlTokenAt ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "name"                      )
      xmlTokenAt ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "main_window"               )
      xmlTokenAt ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenString "width"                     )
      xmlTokenAt ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` Just (XmlTokenNumber 500.0                       )
