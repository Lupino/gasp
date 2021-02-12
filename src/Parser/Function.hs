module Parser.Function
    ( function
    ) where

import qualified Data.Text          as Text
import           Text.Parsec.String (Parser)

import           Gasp.Flag          (initFlag)
import           Gasp.Function
import           Lexer
import           Parser.Common
import           Text.Parsec        (anyChar, manyTill, option, try)

argvParser :: Parser String
argvParser = do
  _ <- symbol "("
  strip <$> manyTill anyChar (try (symbol ")"))

-- func funcName [(argv)] do
--
-- done

function :: Parser Function
function = do
    reserved reservedNameFunction
    name <- identifier
    argv <- option "" argvParser
    code <- gaspBlockClosure


    return Function
        { funcName = name
        , funcCode = Text.pack code
        , funcArgv = argv
        , funcFlag = initFlag name
        }
