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

instance Foldable DecodeResult where
  foldr f z (DecodeOk     a) = f a z
  foldr _ z (DecodeFailed _) = z

instance Traversable DecodeResult where
  traverse _ (DecodeFailed e) = pure (DecodeFailed e)
  traverse f (DecodeOk x)     = DecodeOk <$> f x

toEither :: DecodeResult a -> Either DecodeError a
toEither (DecodeOk     a) = Right a
toEither (DecodeFailed e) = Left  e

isOk :: DecodeResult a -> Bool
isOk (DecodeOk _) = True
isOk _            = False

isFailed :: DecodeResult a -> Bool
isFailed (DecodeFailed _) = True
isFailed _                = False
