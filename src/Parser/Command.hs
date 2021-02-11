module Parser.Command
    ( command
    ) where

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Text          (pack)
import qualified Gasp.Command       as Command
import           Gasp.Flag          (initFlag)
import           Lexer
import           Parser.Common
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Text.Printf        (printf)

-- | A type that describes supported app properties.
data CommandProperty
    = Func !String
    | ErrS !String
    | DocS !String
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
cmdProperties :: Parser [CommandProperty]
cmdProperties = commaSep1 $ cmdPropertyFunc <|> cmdPropertyErrS <|> cmdPropertyDocS

cmdPropertyFunc :: Parser CommandProperty
cmdPropertyFunc = Func <$> gaspProperty "fn" identifier

cmdPropertyErrS :: Parser CommandProperty
cmdPropertyErrS = ErrS <$> gaspPropertyStringLiteral "error"

cmdPropertyDocS :: Parser CommandProperty
cmdPropertyDocS = DocS <$> gaspProperty "doc" gaspBlockClosure

-- TODO(matija): unsafe, what if empty list?
getCmdFunc :: [CommandProperty] -> String
getCmdFunc ps = head $ [t | Func t <- ps]

getCmdErrS :: String -> [CommandProperty] -> String
getCmdErrS err ps = fromMaybe err . listToMaybe $ [t | ErrS t <- ps]

getCmdDocS :: [CommandProperty] -> String
getCmdDocS ps = fromMaybe "" . listToMaybe $ [t | DocS t <- ps]

-- | Top level parser, parses Command.
command :: Parser Command.Command
command = do
    (cmdName, cmdProps) <- gaspElementNameAndClosureContent reservedNameCommand cmdProperties

    return Command.Command
        { Command.cmdName = cmdName
        , Command.cmdFunc = getCmdFunc cmdProps
        , Command.cmdFlag = initFlag (getCmdFunc cmdProps)
        , Command.cmdErrS = getCmdErrS ("call %s failed" `printf` cmdName) cmdProps
        , Command.cmdDocS = pack (getCmdDocS cmdProps)
        }
