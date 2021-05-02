module Parser.Import
  ( importParser
  ) where

import           Gasp.Import
import           Lexer
import           Text.Parsec        (many1, noneOf, option)
import           Text.Parsec.String (Parser)

urlP :: Parser String
urlP = strip <$> many1 (noneOf "\n\r")

importParser :: Parser Import
importParser = do
  reserved reservedNameImport
  name <- many1 (noneOf " \n\r")
  v <- Import name <$> option "" urlP
  whiteSpace
  return v
