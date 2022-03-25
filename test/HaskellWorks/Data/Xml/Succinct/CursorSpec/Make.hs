{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.Succinct.CursorSpec.Make
  ( make
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.Xml.Succinct.Cursor           as C
import HaskellWorks.Data.Xml.Token
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as T
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.FromByteString             as BS
import qualified HaskellWorks.Data.TreeCursor                 as TC
import qualified HaskellWorks.Data.Xml.Succinct.Cursor.Create as CC

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
cd = TC.depth
ss = TC.subtreeSize

make :: forall t u.
  ( Select1           t
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u
  )
  => String -> (BS.ByteString -> XmlCursor BS.ByteString t u) -> SpecWith ()
make t mkCursor = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    let forXml bs f = let cursor = mkCursor bs in describe (T.unpack ("of value " <> T.decodeUtf8 bs)) (f cursor)
    forXml "[null]" $ \cursor -> do
      xit "depth at top"                  $ requireTest $ cd          cursor === Just 1
      xit "depth at first child of array" $ requireTest $ (fc >=> cd) cursor === Just 2
    forXml "[null, {\"field\": 1}]" $ \cursor -> do
      xit "depth at second child of array" $ requireTest $ do
        (fc >=> ns >=> cd) cursor === Just 2
      xit "depth at first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> cd) cursor === Just 3
      xit "depth at first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> ns >=> cd) cursor === Just 3

    describe "For sample XML" $ do
      let cursor = mkCursor "<widget debug=\"on\"> \
                    \  <window name=\"main_window\"> \
                    \    <dimension>500</dimension> \
                    \    <dimension>600.01e-02</dimension> \
                    \    <dimension>    false   </dimension> \
                    \  </window> \
                    \</widget>" :: XmlCursor BS.ByteString t u
      xit "can get token at cursor" $ requireTest $ do
        (xmlTokenAt                                                                      ) cursor === Just (XmlTokenBraceL                 )
        (fc                                                                >=> xmlTokenAt) cursor === Just (XmlTokenString   "widget"      )
        (fc >=> ns                                                         >=> xmlTokenAt) cursor === Just (XmlTokenBraceL                 )
        (fc >=> ns >=> fc                                                  >=> xmlTokenAt) cursor === Just (XmlTokenString   "debug"       )
        (fc >=> ns >=> fc >=> ns                                           >=> xmlTokenAt) cursor === Just (XmlTokenString   "on"          )
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> xmlTokenAt) cursor === Just (XmlTokenString   "window"      )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> xmlTokenAt) cursor === Just (XmlTokenBraceL                 )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> xmlTokenAt) cursor === Just (XmlTokenString   "name"        )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> xmlTokenAt) cursor === Just (XmlTokenString   "main_window" )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> xmlTokenAt) cursor === Just (XmlTokenString   "dimensions"  )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> xmlTokenAt) cursor === Just (XmlTokenBracketL               )
      -- xit "can navigate up" $ requireTest $ do
      --   (                                                                      pn) cursor === Nothing
      --   (fc                                                                >=> pn) cursor ===                                    Just cursor
      --   (fc >=> ns                                                         >=> pn) cursor ===                                    Just cursor
      --   (fc >=> ns >=> fc                                                  >=> pn) cursor === (fc >=> ns                            ) cursor
      --   (fc >=> ns >=> fc >=> ns                                           >=> pn) cursor === (fc >=> ns                            ) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns                                    >=> pn) cursor === (fc >=> ns                            ) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> pn) cursor === (fc >=> ns                            ) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      --   (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      xit "can get subtree size" $ requireTest $ do
        (                                                                      ss) cursor === Just 16
        (fc                                                                >=> ss) cursor === Just 1
        (fc >=> ns                                                         >=> ss) cursor === Just 14
        (fc >=> ns >=> fc                                                  >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns                                           >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> ss) cursor === Just 10
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ss) cursor === Just 6
