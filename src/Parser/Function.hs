module Parser.Function
    ( function
    ) where

import qualified Data.Text          as Text
import           Text.Parsec.String (Parser)

import           Gasp.Flag          (initFlag)
import           Gasp.Function
import           Lexer
import           Parser.Common
import           Text.Parsec        (many1, noneOf, option)

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
    name  <- identifier
    argv  <- option ""     $ block "(" ")"
    tp    <- option "void" tpParser
    code  <- gaspBlockClosure


    return Function
        { funcName = FuncName name
        , funcCode = Text.pack code
        , funcArgv = argv
        , funcFlag = initFlag name
        , funcType = tp
        }
