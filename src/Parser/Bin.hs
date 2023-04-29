module Parser.Bin
    ( bin
    ) where

import           Gasp.Bin
import           Lexer
import           Text.Parsec        ((<|>))
import           Text.Parsec.String (Parser)

bin' :: Parser Bin
bin' = do
  reserved reservedNameBin
  bn <- BinName <$> identifier
  Bin bn "uint8_t" 1 0 . fromIntegral <$> integer

str :: Parser Bin
str = do
  reserved reservedNameStr
  bn <- BinName <$> identifier
  Bin bn "char" 1 0 . fromIntegral <$> integer

struct :: Parser Bin
struct = do
  reserved reservedNameStruct
  tp <- identifier
  len <- fromIntegral <$> integer
  bn <- BinName <$> identifier
  Bin bn tp len 0 . fromIntegral <$> integer


bin :: Parser Bin
bin = bin' <|> struct <|> str
