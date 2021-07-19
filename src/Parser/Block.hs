module Parser.Block
  ( blockP
  , blockV
  , setup
  , loop
  , raw
  , data_
  ) where

import           Data.Aeson         (Value)
import qualified Data.Text          as T
import           Gasp.Block
import qualified Lexer              as L
import qualified Parser.Common      as P
import           Text.Parsec.String (Parser)

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
loop  = blockP L.reservedNameLoop  Loop

raw :: Parser Raw
raw = blockP L.reservedNameRaw Raw

data_ :: Parser Data
data_ = blockV L.reservedNameData Data
