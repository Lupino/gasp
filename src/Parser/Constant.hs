module Parser.Constant
  ( constant
  ) where

import           Gasp.Constant
import           Lexer
import           Text.Parsec        (many, noneOf, (<|>))
import           Text.Parsec.String (Parser)

tpParser :: Parser String
tpParser = strip <$> many (noneOf "=")

-- | name [type] = value
constant :: Parser Constant
constant = do
  name <- identifier
  tp   <- tpParser
  _ <- symbol "="
  value <- identifier <|> (show <$> float)

  return Constant
    { constName  = name
    , constValue = value
    , constType  = tp
    }
