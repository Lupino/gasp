module Parser.Function
    ( function
    ) where

import qualified Data.Text          as Text
import           Text.Parsec.String (Parser)

import           Gasp.Flag          (initFlag)
import           Gasp.Function
import qualified Lexer              as L
import qualified Parser.Common      as P

function :: Parser Function
function = do
    (name, code) <- P.gaspElementNameAndClosure L.reservedNameFunction
                                                          (P.gaspNamedClosure "code")

    return Function
        { funcName = name
        , funcCode = Text.pack code
        , funcFlag = initFlag name
        }
