{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Decode where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Text                           (Text)
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.Internal.Show
import HaskellWorks.Data.Xml.Value

class Decode a where
  decode :: Value -> DecodeResult a

instance Decode Value where
  decode = DecodeOk
  {-# INLINE decode #-}

failDecode :: Text -> DecodeResult a
failDecode = DecodeFailed . DecodeError

(@>) :: Value -> Text -> DecodeResult Text
(@>) (XmlElement _ as _) n = case find (\v -> fst v == n) as of
  Just (_, text) -> DecodeOk text
  Nothing        -> failDecode $ "No such attribute " <> tshow n
(@>) _ n = failDecode $ "Not an element whilst looking up attribute " <> tshow n

(/>) :: Value -> Text -> DecodeResult Value
(/>) (XmlElement _ _ cs) n = go cs
  where go []   = failDecode $ "Unable to find element " <> tshow n
        go (r:rs) = case r of
          e@(XmlElement n' _ _) | n' == n -> DecodeOk e
          _                               -> go rs
(/>) _ n = failDecode $ "Expecting parent of element " <> tshow n

(?>) :: Value -> (Value -> DecodeResult Value) -> DecodeResult Value
(?>) v f = f v <|> pure v

(~>) :: Value -> Text -> DecodeResult Value
(~>) e@(XmlElement n' _ _)  n | n' == n = DecodeOk e
(~>) _                      n = failDecode $ "Expecting parent of element " <> tshow n

(/>>) :: Value -> Text -> DecodeResult [Value]
(/>>) v n = v ^. childNodes <&> (~> n) <&> toList & join & pure

-- Contextful

(</>) :: DecodeResult Value -> Text -> DecodeResult Value
(</>) ma n = ma >>= (/> n)

(<@>) :: DecodeResult Value -> Text -> DecodeResult Text
(<@>) ma n = ma >>= (@> n)

(<?>) :: DecodeResult Value -> (Value -> DecodeResult Value) -> DecodeResult Value
(<?>) ma f = ma >>= (?> f)
