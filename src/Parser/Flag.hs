module Parser.Flag
  ( flag
  ) where

import           Data.Text          (pack)
import           Gasp.Flag
import           Lexer
import           Text.Parsec.String (Parser)

flag :: Parser Flag
flag = do
  reserved reservedNameFlag
  name <- pack <$> identifier
  Flag name <$> bool
