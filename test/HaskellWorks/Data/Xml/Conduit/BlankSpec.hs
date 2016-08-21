{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Conduit.BlankSpec (spec) where

import qualified Data.ByteString                     as BS
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Xml.Conduit.Blank
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedXmlShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedXmlShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ do
    BS.concat (runListConduit blankXml [original]) `shouldBe` expected

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Conduit.BlankSpec" $ do
  describe "Can blank XML" $ do
    "<b/>"                                    `whenBlankedXmlShouldBe` "<  >"
    "<b></b>"                                 `whenBlankedXmlShouldBe` "<     >"
    "<b>text</b>"                             `whenBlankedXmlShouldBe` "<  (  )   >"
    "<b>  text  </b>"                         `whenBlankedXmlShouldBe` "<    (    )   >"
    "<b />"                                   `whenBlankedXmlShouldBe` "< ()>"
    "<foo bar='buzz' />"                      `whenBlankedXmlShouldBe` "<   ((  )(    ) )>"
    "<foo xsd:bar='buzz' />"                  `whenBlankedXmlShouldBe` "<   ((      )(    ) )>"
    "<foo bar=\"buzz\" />"                    `whenBlankedXmlShouldBe` "<   ((  )(    ) )>"
    "<e a='x' b='y'/>"                        `whenBlankedXmlShouldBe` "< (()( ) ()( ))>"
    "<e a='x' b='y'/>"                        `whenBlankedXmlShouldBe` "< (()( ) ()( ))>"
    "<e a='x' b='y'>text</e>"                 `whenBlankedXmlShouldBe` "< (()( ) ()( ))(  )   >"
    "<e a = 'x' b = 'y' />"                   `whenBlankedXmlShouldBe` "< (()  ( ) ()  ( ) )>"
    "<a x='y'><b/></a>"                       `whenBlankedXmlShouldBe` "< (()( ))<  >   >"
    "<a x='y'><b>test</b></a>"                `whenBlankedXmlShouldBe` "< (()( ))<  (  )   >   >"
    "<test> text <b>bold</b> </test>"         `whenBlankedXmlShouldBe` "<      (   )<  (  )   >       >"
    "<test> text <b>bold</b> uuu</test>"      `whenBlankedXmlShouldBe` "<      (   )<  (  )   > ( )      >"
    "<person fstName=\"alexey\" />"           `whenBlankedXmlShouldBe` "<      ((      )(      ) )>"
    "<e> <!-- comment --> </e>"               `whenBlankedXmlShouldBe` "<   [              ]    >"
    "<e> <!-- a --- z --> </e>"               `whenBlankedXmlShouldBe` "<   [              ]    >"

    "<e> <!-- <b>a</b> --> </e>"              `whenBlankedXmlShouldBe` "<   [               ]    >"
    "<?xml version='1.0' encoding='UTF-8' ?>" `whenBlankedXmlShouldBe` "<    ((      )(   ) (       )(     ) )>"

    "<!DOCTYPE greeting [\
      \<!ELEMENT greeting (#PCDATA)>]>"       `whenBlankedXmlShouldBe` "[                   [                           ] ]"

    "<a><![CDATA[<hi>Hello,\
    \ world!</hi>]]></b>"                     `whenBlankedXmlShouldBe` "<  [                                ]   >"

    "<a><![CDATA[ [ ]]]]></b>"                `whenBlankedXmlShouldBe` "<  [               ]   >"
