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

import           Control.Monad
import qualified Data.ByteString                                            as BS
import qualified Data.Map                                                   as M
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Xml.Succinct.Cursor                     as C
import           HaskellWorks.Data.Xml.Token
import           HaskellWorks.Data.Xml.Value
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
      xmlCursorType cursor `shouldBe` Just XmlCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlCursorType cursor `shouldBe` Just XmlCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlCursorType) cursor `shouldBe` Just XmlCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> xmlCursorType) cursor `shouldBe` Just XmlCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> ns >=> xmlCursorType) cursor `shouldBe` Just XmlCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` Just 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> cd) cursor `shouldBe` Just 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> cd) cursor `shouldBe` Just 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
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
  , HasXmlCursorType (XmlCursor BS.ByteString t u)
  , XmlValueAt BS.ByteString BS.ByteString (XmlCursor BS.ByteString t u))
  => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorObject
      xmlValueAt cursor `shouldBe` Just (XmlObject M.empty :: XmlValue BS.ByteString BS.ByteString)
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorArray
      xmlValueAt cursor `shouldBe` Just (XmlArray [] :: XmlValue BS.ByteString BS.ByteString)
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: XmlCursor BS.ByteString t u
      xmlCursorType cursor `shouldBe` Just XmlCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      (fc >=> xmlCursorType) cursor `shouldBe` Just XmlCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> xmlCursorType) cursor `shouldBe` Just XmlCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> ns >=> xmlCursorType) cursor `shouldBe` Just XmlCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      cd cursor `shouldBe` Just 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: XmlCursor BS.ByteString t u
      (fc >=> cd) cursor `shouldBe` Just 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> cd) cursor `shouldBe` Just 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: XmlCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      (xmlCursorType                                                                                    ) cursor `shouldBe` Just XmlCursorObject
      (fc                                                                              >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns                                                                       >=> xmlCursorType) cursor `shouldBe` Just XmlCursorObject
      (fc >=> ns >=> fc                                                                >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns                                                         >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns                                                  >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           >=> xmlCursorType) cursor `shouldBe` Just XmlCursorObject
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        >=> xmlCursorType) cursor `shouldBe` Just XmlCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> xmlCursorType) cursor `shouldBe` Just XmlCursorNumber
    it "can navigate up" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      (fc >=> pn                                                                             ) cursor `shouldBe`                                    Just cursor
      (fc >=> ns >=> pn                                                                      ) cursor `shouldBe`                                    Just cursor
      (fc >=> ns >=> fc >=> pn                                                               ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> pn                                                        ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> pn                                                 ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn                                          ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> pn                                   ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> pn                            ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> pn                     ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn              ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> pn       ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
    it "can get subtree size" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      ss                                                                                       cursor `shouldBe` Just 45
      (fc                                                                              >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns                                                                       >=> ss) cursor `shouldBe` Just 43
      (fc >=> ns >=> fc                                                                >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns                                                         >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns                                                  >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           >=> ss) cursor `shouldBe` Just 9
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        >=> ss) cursor `shouldBe` Just 1
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> ss) cursor `shouldBe` Just 1
    it "can get token at cursor" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.xml" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: XmlCursor BS.ByteString t u
      (xmlTokenAt                                                                                    ) cursor `shouldBe` Just (XmlTokenBraceL                             )
      (fc                                                                              >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "widget"                    )
      (fc >=> ns                                                                       >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenBraceL                             )
      (fc >=> ns >=> fc                                                                >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "debug"                     )
      (fc >=> ns >=> fc >=> ns                                                         >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "on"                        )
      (fc >=> ns >=> fc >=> ns >=> ns                                                  >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "window"                    )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenBraceL                             )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "title"                     )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "Sample Konfabulator Widget")
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "name"                      )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "main_window"               )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString "width"                     )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenNumber 500.0                       )
