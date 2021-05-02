module Parser.Function
    ( function
    ) where

import qualified Data.Text          as Text
import           Text.Parsec.String (Parser)

import           Gasp.Flag          (initFlag)
import           Gasp.Function
import           Lexer
import           Parser.Common
import           Text.Parsec        (many, noneOf, option)

tpParser :: Parser String
tpParser = fixed . strip <$> many (noneOf "{}")
  where fixed [] = "bool"
        fixed v  = v

-- func funcName [(argv)] [rettp] do
--
-- done

function :: Parser Function
function = do
    reserved reservedNameFunction
    name  <- identifier
    argv  <- option ""     $ block "(" ")"
    rettp <- option "bool" tpParser
    code  <- gaspBlockClosure


    return Function
        { funcName = FuncName name
        , funcCode = Text.pack code
        , funcArgv = argv
        , funcFlag = initFlag name
        , funcRet  = rettp
        }
