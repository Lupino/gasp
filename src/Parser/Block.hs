module Parser.Block
  ( anyName
  , normalName
  , readEndOfLine
  , block_
  , blockLine
  , blockBool
  , blockP
  , blockV
  , setup
  , loop
  , setup1
  , loop1
  , raw
  , data_
  , tmpl
  , render
  , render1
  , require
  , import_
  , flag
  , fd
  , ifeq
  , ifneq
  ) where

import           Data.Aeson         (Value)
import qualified Data.Text          as T
import           Gasp.Block
import qualified Lexer              as L
import qualified Parser.Common      as P
import           Text.Parsec        (many1, noneOf, option, (<|>))
import           Text.Parsec.String (Parser)


anyName :: Parser String
anyName = many1 (noneOf " \n\r")

normalName :: Parser String
normalName = do
  v <- L.stringLiteral <|> anyName
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
  name <- anyName
  r <- f name <$> option "" readEndOfLine
  L.whiteSpace
  return r

blockBool :: String -> (String -> Bool -> a) -> Parser a
blockBool reservedName f = do
  L.reserved reservedName
  name <- normalName
  f name <$> L.bool

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
setup = blockP L.reservedNameSetup Setup <|> blockP L.reservedNameSetup0 Setup

loop :: Parser Loop
loop = blockP L.reservedNameLoop Loop <|> blockP L.reservedNameLoop0 Loop

setup1 :: Parser Setup1
setup1 = blockP L.reservedNameSetup1 Setup1

loop1 :: Parser Loop1
loop1 = blockP L.reservedNameLoop1 Loop1

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

flag :: Parser Flag
flag = blockBool L.reservedNameFlag Flag

fd :: Parser Fd
fd = do
  L.reserved L.reservedNameFd
  fd_ <- fromIntegral <$> L.integer
  Fd fd_ <$> normalName

ifeq :: Parser IfEq
ifeq = blockP L.reservedNameIfEq IfEq

ifneq :: Parser IfNeq
ifneq = blockP L.reservedNameIfNeq IfNeq
