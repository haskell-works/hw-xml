module App.XPath.Parser
  ( path
  ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text            (Text)

import qualified App.XPath.Types as XP
import qualified Data.Text       as T

tag :: Parser Text
tag = T.cons <$> letter <*> tagTail

tagTail :: Parser Text
tagTail = T.pack <$> many (letter <|> digit <|> char '-' <|> char '_')

path :: Parser XP.XPath
path = XP.XPath <$> sepBy1 tag (char '/')
