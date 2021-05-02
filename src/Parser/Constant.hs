module Parser.Constant
  ( constant
  ) where

import           Gasp.Constant
import           Lexer
import           Text.Parsec        (many, noneOf, option)
import           Text.Parsec.String (Parser)

tpParser :: Parser String
tpParser = strip <$> many (noneOf "=\n\r")

valueParser :: Parser String
valueParser = do
  _    <- symbol "="
  strip <$> many (noneOf "\n\r")


-- | name [type] = value
constant :: Parser Constant
constant = do
  name  <- identifier
  argv  <- option "" $ block "(" ")"
  tp    <- tpParser
  whiteSpace
  value <- option "" valueParser
  whiteSpace

  return Constant
    { constName  = name
    , constValue = value
    , constArgv  = argv
    , constType  = tp
    }
