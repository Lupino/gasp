module Parser.Constant
  ( constant
  ) where

import           Gasp.Constant
import           Lexer
import           Text.Parsec        (many, noneOf, option, (<|>))
import           Text.Parsec.String (Parser)

tpParser :: Parser String
tpParser = strip <$> many (noneOf "=\n\r")

valueParser :: Parser String
valueParser = do
  _    <- symbol "="
  whiteSpace
  strip <$> (argvP '{' '}' <|> many (noneOf "\n\r"))


argvP :: Char -> Char -> Parser String
argvP s e = do
  argv <- blockC s e
  whiteSpace
  return $ [s] ++ argv ++ [e]


-- | name [type] = value
constant :: Parser Constant
constant = do
  name  <- identifier
  argv0 <- option "" $ argvP '[' ']'
  argv1 <- option "" $ argvP '(' ')'
  tp    <- tpParser
  whiteSpace
  value <- option "" valueParser
  whiteSpace

  return Constant
    { constName  = name ++ argv0 ++ argv1
    , constValue = value
    , constType  = tp
    }
