module Parser.Gpio
    ( gpio
    ) where

import           Gasp.Gpio
import           Lexer
import           Text.Parsec        (option, (<|>))
import           Text.Parsec.String (Parser)

bindParser :: Gpio -> Parser Gpio
bindParser g = do
  _ <- symbol "->"
  sym <- symbol "click" <|> symbol "link"
  n <- identifier

  case sym of
    "link" -> do
      v <- option False bool
      return g
        { gpioLink = n
        , gpioReverse = v
        }
    "click" -> do
      v <- option (gpioEmit g) identifier
      return g
        { gpioFunc = n
        , gpioEmit = v
        }

    _ -> fail $ "no such symbol " ++ sym

--                       default    open                        reverse
-- gpio gpioName pinName [LOW|HIGH] [LOW|HIGH] -> link attrName [false|true]
--                       default                      emit
-- gpio gpioName pinName [LOW|HIGH] -> click funcName [LOW|HIGH]

-- | Top level parser, parses Gpio.
gpio :: Parser Gpio
gpio = do
  reserved reservedNameGpio
  name <- identifier
  pin <- stringLiteral <|> (show <$> integer) <|> identifier
  state <- option "LOW" identifier
  let revertState = if state == "LOW" then "HIGH" else "LOW"
  open <- option revertState identifier
  let close = if open == "LOW" then "HIGH" else "LOW"

  let g = Gpio
        { gpioName    = name
        , gpioPin     = pin
        , gpioLink    = ""
        , gpioReverse = False
        , gpioFunc    = ""
        , gpioEmit    = revertState
        , gpioState   = state
        , gpioOpen    = open
        , gpioClose   = close
        }

  option g $ bindParser g
