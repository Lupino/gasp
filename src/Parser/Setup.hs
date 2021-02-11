module Parser.Setup
  ( setup
  ) where

import           Text.Parsec.String (Parser)

import qualified Data.Text          as Text
import           Gasp.Setup
import qualified Lexer              as L
import qualified Parser.Common      as P

setup :: Parser Setup
setup = do
    L.reserved L.reservedNameSetup
    code <- P.gaspBlockClosure

    return Setup
      { setupCode = Text.pack code
      }
