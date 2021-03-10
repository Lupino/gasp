module Parser.Gpio
  ( gpio
  ) where

import           Gasp.Gpio
import           Lexer
import           Text.Parsec        (option, spaces, (<|>))
import           Text.Parsec.String (Parser)

bindClick :: State -> Parser GpioBind
bindClick emit = do
  _ <- symbol "click"
  n <- FuncName <$> identifier
  v <- State <$> option (unState emit) (stateLow <|> stateHigh)
  return $ CallFn n v

bindLink :: Parser GpioBind
bindLink = do
  _ <- symbol "link"
  n <- AttrName <$> identifier
  v <- option False bool
  return $ LinkAttr n v

bindPwm :: Parser GpioBind
bindPwm = do
  _ <- symbol "pwm"
  n <- AttrName <$> identifier
  return $ PwmAttr n

bindParser :: State -> Parser GpioBind
bindParser emit = do
  _ <- symbol "->"
  bindLink <|> bindClick emit <|> bindPwm

--                       default   open                          reverse
-- gpio gpioName pinName [LOW|HIGH [LOW|HIGH]] [-> link attrName [false|true]]
--                       default                       emit
-- gpio gpioName pinName [LOW|HIGH] [-> click funcName [LOW|HIGH]]
--                       default
-- gpio gpioName pinName [NUM] [-> pwm attrName]

stateLow :: Parser String
stateLow = symbol "LOW"

stateHigh :: Parser String
stateHigh = symbol "HIGH"

stateNum :: Parser String
stateNum = do
  v <- show <$> decimal
  spaces
  return v


-- | Top level parser, parses Gpio.
gpio :: Parser Gpio
gpio = do
  reserved reservedNameGpio
  name <- identifier
  pin <- stringLiteral <|> (show <$> integer) <|> identifier
  state <- option "LOW" (stateLow <|> stateHigh <|> stateNum)
  let revertState = if state == "LOW" then "HIGH" else "LOW"
  open <- option revertState (stateLow <|> stateHigh)
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
