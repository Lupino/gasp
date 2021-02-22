module Parser.Constant
  ( constant
  ) where

import           Gasp.Constant
import           Lexer
import           Text.Parsec        ((<|>))
import           Text.Parsec.String (Parser)

-- | name = value
constant :: Parser Constant
constant = do
  name <- identifier
  _ <- symbol "="
  value <- identifier <|> (show <$> float)

  return Constant
    { constName  = name
    , constValue = value
    }
