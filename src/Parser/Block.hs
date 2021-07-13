module Parser.Block
  ( blockP
  , setup
  , loop
  ) where

import qualified Data.Text          as T
import           Gasp.Block
import qualified Lexer              as L
import qualified Parser.Common      as P
import           Text.Parsec.String (Parser)

blockP :: String -> (T.Text -> a) -> Parser a
blockP reservedName f = do
  L.reserved reservedName
  f . T.pack <$> P.gaspBlockClosure

setup :: Parser Setup
setup = blockP L.reservedNameSetup Setup

loop :: Parser Loop
loop  = blockP L.reservedNameLoop  Loop
