module HaskellWorks.Data.Xml.Decode where

import Control.Applicative
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

(@>) :: Value -> String -> DecodeResult String
(@>) (XmlElement _ as _) n = case M.lookup n as of
  Just text -> DecodeOk text
  Nothing   -> failDecode $ "No such attribute " <> show n
(@>) _ n = failDecode $ "Not an element whilst looking up attribute " <> show n

(/>) :: Value -> String -> DecodeResult Value
(/>) (XmlElement _ _ cs) n = go cs
  where go []   = failDecode $ "Unable to find element " <> show n
        go (r:rs) = case r of
          e@(XmlElement n' _ _) | n' == n -> DecodeOk e
          _                               -> go rs
(/>) _ n = failDecode $ "Expecting parent of element " <> show n

(?>) :: Value -> (Value -> DecodeResult Value) -> DecodeResult Value
(?>) v f = f v <|> pure v

-- Contextful

(</>) :: DecodeResult Value -> String -> DecodeResult Value
(</>) ma n = ma >>= (/> n)

(<@>) :: DecodeResult Value -> String -> DecodeResult String
(<@>) ma n = ma >>= (@> n)

(<?>) :: DecodeResult Value -> (Value -> DecodeResult Value) -> DecodeResult Value
(<?>) ma f = ma >>= (?> f)

-- Deprecated
(~@) :: Value -> String -> DecodeResult String
(~@) = (@>)

(/*) :: Value -> String -> DecodeResult Value
(/*) = (/>)
