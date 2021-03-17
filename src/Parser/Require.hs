module Parser.Require
  ( require
  ) where

import           Gasp.Require
import           Lexer
import           Text.Parsec.String (Parser)

require :: Parser Require
require = do
  reserved reservedNameRequire
  Require <$> stringLiteral
