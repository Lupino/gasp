module Parser.Init
  ( initP
  ) where

import           Text.Parsec.String (Parser)

import qualified Data.Text          as Text
import           Gasp.Init
import qualified Lexer              as L
import qualified Parser.Common      as P

initP :: Parser Init
initP = do
    L.reserved L.reservedNameInit
    code <- P.gaspNamedClosure "code"

    return Init
      { initCode = Text.pack code
      }
