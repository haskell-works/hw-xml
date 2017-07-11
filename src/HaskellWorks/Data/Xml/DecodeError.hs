{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Xml.DecodeError where

newtype DecodeError = DecodeError String deriving (Eq, Show)
