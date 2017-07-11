module HaskellWorks.Data.Xml.Decode where

import Data.Monoid                       ((<>))
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.Value

import qualified Data.Map as M

class Decode a where
  decode :: Value -> Either DecodeError a

instance Decode Value where
  decode = Right
  {-# INLINE decode #-}

failDecode :: String -> Either DecodeError a
failDecode = Left . DecodeError

(~@) :: Value -> String -> Either DecodeError String
(~@) (XmlElement _ as _) name = case M.lookup name as of
  Just text -> Right text
  Nothing   -> failDecode $ "No such attribute " <> show name
(~@) _ name = failDecode $ "Not an element whilst looking up attribute " <> show name
