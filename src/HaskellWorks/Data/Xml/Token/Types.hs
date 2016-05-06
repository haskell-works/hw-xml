module HaskellWorks.Data.Xml.Token.Types (XmlToken(..)) where

data XmlToken s d
  = XmlTokenBraceL
  | XmlTokenBraceR
  | XmlTokenBracketL
  | XmlTokenBracketR
  | XmlTokenComma
  | XmlTokenColon
  | XmlTokenWhitespace
  | XmlTokenString s
  | XmlTokenBoolean Bool
  | XmlTokenNumber d
  | XmlTokenNull
  deriving (Eq, Show)
