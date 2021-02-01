module Parser.Monitor
  ( monitor
  ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Gasp.Monitor
import           Lexer
import           Parser.Common

monitor :: Parser Monitor
monitor = do
    (name, props) <- gaspElementNameAndClosureContent reservedNameMonitor monProperties

    return Monitor
        { monFunc    = getMonFunc props
        , monDelayMs = getMonDelayMs props
        , monName    = name
        }

-- Auxiliary data structure used by parser.
data MonitorProperty = Func String | DelayMs String

getMonFunc :: [MonitorProperty] -> String
getMonFunc props = head [s | Func s <- props]

getMonDelayMs :: [MonitorProperty] -> String
getMonDelayMs props = fromMaybe "5000" . listToMaybe $ [s | DelayMs s <- props]

monProperties :: Parser [MonitorProperty]
monProperties = commaSep1 $ monPropertyFunc <|> monPropertyDelayMs

monPropertyFunc :: Parser MonitorProperty
monPropertyFunc = Func <$> gaspProperty "fn" identifier

monPropertyDelayMs :: Parser MonitorProperty
monPropertyDelayMs = DelayMs <$> gaspProperty "delay_ms" integerString
