module Parser.Block
  ( normalName
  , readEndOfLine
  , block_
  , blockLine
  , blockP
  , blockV
  , setup
  , loop
  , raw
  , data_
  , tmpl
  , render
  , render1
  , require
  , import_
  ) where

import           Data.Aeson         (Value)
import qualified Data.Text          as T
import           Gasp.Block
import qualified Lexer              as L
import qualified Parser.Common      as P
import           Text.Parsec        (many1, noneOf, option)
import           Text.Parsec.String (Parser)


normalName0 :: Parser String
normalName0 = many1 (noneOf " \n\r")

normalName :: Parser String
normalName = do
  v <- normalName0
  L.whiteSpace
  return v

readEndOfLine :: Parser String
readEndOfLine = L.strip <$> many1 (noneOf "\n\r")

block_ :: String -> (String -> a) -> Parser a
block_ reservedName f = do
  L.reserved reservedName
  f <$> normalName

blockLine :: String -> (String -> String -> a) -> Parser a
blockLine reservedName f = do
  L.reserved reservedName
  name <- normalName0
  r <- f name <$> option "" readEndOfLine
  L.whiteSpace
  return r

blockP :: String -> (String -> T.Text -> a) -> Parser a
blockP reservedName f = do
  L.reserved reservedName
  name  <- normalName
  f name . T.pack <$> P.gaspBlockClosure

blockV :: String -> (String -> Value -> a) -> Parser a
blockV reservedName f = do
  L.reserved reservedName
  name  <- normalName
  f name <$> L.jsonObject


setup :: Parser Setup
setup = blockP L.reservedNameSetup Setup

loop :: Parser Loop
loop = blockP L.reservedNameLoop Loop

raw :: Parser Raw
raw = blockP L.reservedNameRaw Raw

data_ :: Parser Data
data_ = blockV L.reservedNameData Data

tmpl :: Parser Tmpl
tmpl = blockP L.reservedNameTmpl Tmpl

render :: Parser Render
render = block_ L.reservedNameRender Render

render1 :: Parser Render1
render1  = blockV L.reservedNameRender1 Render1

require :: Parser Require
require = block_ L.reservedNameRequire Require

import_ :: Parser Import
import_ = blockLine L.reservedNameImport Import
