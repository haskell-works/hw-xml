module App.Options
  ( optionParser
  ) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text            as T
import qualified Options.Applicative  as OA

optionParser :: AT.Parser a -> OA.Mod OA.OptionFields a -> OA.Parser a
optionParser p = OA.option (OA.eitherReader (AT.parseOnly p . T.pack))
