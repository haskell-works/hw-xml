{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.FromXmlCursor where

import qualified Data.ByteString as BS
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Xml.Succinct.Cursor.Internal
import           HaskellWorks.Data.Xml.Succinct.Cursor.Token
import           HaskellWorks.Data.Xml.Value
import           HaskellWorks.Data.Xml.Token.Types
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.TreeCursor

class FromXmlCursor t v w a where
  fromXmlCursor :: XmlCursor t v w -> a

instance (TreeCursor (XmlCursor BS.ByteString v w), Rank1 w, Select1 v, TestBit w) => FromXmlCursor BS.ByteString v w (XmlValue BS.ByteString BS.ByteString) where
  fromXmlCursor k = case xmlTokenAt k of
    Just XmlTokenBraceL          -> undefined
    Just XmlTokenBraceR          -> undefined
    Just XmlTokenBracketL        -> undefined
    Just XmlTokenBracketR        -> undefined
    Just XmlTokenComma           -> undefined
    Just XmlTokenColon           -> undefined
    Just XmlTokenWhitespace      -> undefined
    Just (XmlTokenString _)      -> undefined
    Just (XmlTokenBoolean value) -> XmlBool value
    Just (XmlTokenNumber _)      -> undefined
    Just XmlTokenNull            -> undefined
    Nothing                       -> undefined
