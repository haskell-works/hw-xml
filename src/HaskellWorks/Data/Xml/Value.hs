{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value
  ( Value(..)
  ) where

import qualified Data.Map                         as M

data Value
  = ValueDocument [Value]
  | ValueText String
  | ValueElement String (M.Map String String) [Value]
  | ValueCData String
  | ValueComment String
  | ValueMeta String [Value]
  | ValueError String
  deriving (Eq, Show)
