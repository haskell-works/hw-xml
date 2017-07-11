module HaskellWorks.Data.Xml.Decode where

import Data.Monoid                       ((<>))
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.Value

import qualified Data.Map as M

class Decode a where
  decode :: Value -> DecodeResult a

instance Decode Value where
  decode = DecodeOk
  {-# INLINE decode #-}

failDecode :: String -> DecodeResult a
failDecode = DecodeFailed . DecodeError

(~@) :: Value -> String -> DecodeResult String
(~@) (XmlElement _ as _) name = case M.lookup name as of
  Just text -> DecodeOk text
  Nothing   -> failDecode $ "No such attribute " <> show name
(~@) _ name = failDecode $ "Not an element whilst looking up attribute " <> show name

(/*) :: Value -> String -> DecodeResult Value
(/*) (XmlElement _ _ cs) name = go cs
  where go []   = failDecode $ "Unable to find element " <> show name
        go (r:rs) = case r of
          e@(XmlElement name' _ _) | name' == name  -> DecodeOk e
          _                                         -> go rs
(/*) _ name = failDecode $ "Expecting parent of element " <> show name
