module App.Options
  ( optionParser
  , textOption
  ) where

import Data.Text (Text)

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text            as T
import qualified Options.Applicative  as OA

optionParser :: AT.Parser a -> OA.Mod OA.OptionFields a -> OA.Parser a
optionParser p = OA.option (OA.eitherReader (AT.parseOnly p . T.pack))

textOption :: OA.Mod OA.OptionFields String -> OA.Parser Text
textOption = fmap T.pack . OA.strOption
