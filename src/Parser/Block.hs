module Parser.Block
  ( block_
  , blockP
  , blockV
  , setup
  , loop
  , raw
  , data_
  , tmpl
  , render
  , render1
  ) where

import           Data.Aeson         (Value)
import qualified Data.Text          as T
import           Gasp.Block
import qualified Lexer              as L
import qualified Parser.Common      as P
import           Text.Parsec.String (Parser)

block_ :: String -> (String -> a) -> Parser a
block_ reservedName f = do
  L.reserved reservedName
  f <$> L.identifier

blockP :: String -> (String -> T.Text -> a) -> Parser a
blockP reservedName f = do
  L.reserved reservedName
  name  <- L.identifier
  f name . T.pack <$> P.gaspBlockClosure

blockV :: String -> (String -> Value -> a) -> Parser a
blockV reservedName f = do
  L.reserved reservedName
  name  <- L.identifier
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
