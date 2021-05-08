module Parser.Function
    ( function
    ) where

import qualified Data.Text          as Text
import           Gasp.Function
import           Lexer
import           Parser.Common
import           Text.Parsec        (many1, noneOf, option)
import           Text.Parsec.String (Parser)

tpParser :: Parser String
tpParser = fixed . strip <$> many1 (noneOf "{}")
  where fixed [] = "void"
        fixed v  = v

-- func funcName [(argv)] [rettp] do
--
-- done

function :: Parser Function
function = do
    reserved reservedNameFunction
    name  <- FuncName <$> identifier
    argv  <- option ""     $ block "(" ")"
    tp    <- option "void" tpParser
    code  <- gaspBlockClosure


    return Function
        { funcName = name
        , funcCode = Text.pack code
        , funcArgv = argv
        , funcFlag = genFuncFlag name
        , funcType = tp
        }
