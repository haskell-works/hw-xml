{-# LANGUAGE DeriveFunctor #-}

module HaskellWorks.Data.Xml.DecodeResult where

import Control.Applicative
import HaskellWorks.Data.Xml.DecodeError

data DecodeResult a
  = DecodeOk a
  | DecodeFailed DecodeError
  deriving (Eq, Show, Functor)

instance Applicative DecodeResult where
  pure = DecodeOk
  {-# INLINE pure #-}

  (<*>) (DecodeOk f    ) (DecodeOk     a) = DecodeOk (f a)
  (<*>) (DecodeOk _    ) (DecodeFailed e) = DecodeFailed e
  (<*>) (DecodeFailed e) _                = DecodeFailed e
  {-# INLINE (<*>) #-}

instance Monad DecodeResult where
  return = DecodeOk
  {-# INLINE return #-}

  (>>=) (DecodeOk     a) f = f a
  (>>=) (DecodeFailed e) _ = DecodeFailed e
  {-# INLINE (>>=) #-}

instance Alternative DecodeResult where
  empty = DecodeFailed (DecodeError "Failed decode")
  (<|>) (DecodeOk a) _                = DecodeOk     a
  (<|>) _            (DecodeOk     b) = DecodeOk     b
  (<|>) _            (DecodeFailed e) = DecodeFailed e
  {-# INLINE (<|>) #-}
