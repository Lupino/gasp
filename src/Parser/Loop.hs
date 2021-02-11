module Parser.Loop
  ( loop
  ) where

import           Text.Parsec.String (Parser)

import qualified Data.Text          as Text
import           Gasp.Loop
import qualified Lexer              as L
import qualified Parser.Common      as P

loop :: Parser Loop
loop = do
    L.reserved L.reservedNameLoop
    code <- P.gaspBlockClosure

    return Loop
      { loopCode = Text.pack code
      }
