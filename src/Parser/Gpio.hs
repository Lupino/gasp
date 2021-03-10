module Parser.Gpio
  ( gpio
  ) where

import           Gasp.Gpio
import           Lexer
import           Text.Parsec        (option, (<|>))
import           Text.Parsec.String (Parser)

bindParser :: State -> Parser GpioBind
bindParser emit = do
  _ <- symbol "->"
  sym <- symbol "click" <|> symbol "link"
  n <- identifier

  case sym of
    "link" -> do
      v <- option False bool
      return $ LinkAttr (AttrName n) v
    "click" -> do
      v <- option (unState emit) identifier
      return $ CallFn (FuncName n) (State v)
    _ -> fail $ "no such symbol " ++ sym

--                       default   open                          reverse
-- gpio gpioName pinName [LOW|HIGH [LOW|HIGH]] [-> link attrName [false|true]]
--                       default                       emit
-- gpio gpioName pinName [LOW|HIGH] [-> click funcName [LOW|HIGH]]

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
  b <- option NoBind (bindParser (State revertState))

  return Gpio
    { gpioName  = name
    , gpioPin   = pin
    , gpioState = State state
    , gpioOpen  = State open
    , gpioClose = State close
    , gpioBind  = b
    }
