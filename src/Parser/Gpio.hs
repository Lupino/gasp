module Parser.Gpio
  ( gpio
  , pin
  ) where

import           Gasp.Attr          (AttrName (..))
import           Gasp.Function      (FuncName (..))
import           Gasp.Gpio
import           Lexer
import           Text.Parsec        (option, (<|>))
import           Text.Parsec.String (Parser)

pinName :: Parser Pin
pinName = PinName <$> identifier

pinNum :: Parser Pin
pinNum = do
  v <- decimal
  whiteSpace
  return $ PinNum v

pin :: Parser Pin
pin = pinNum <|> pinName

bindClick :: State -> Parser GpioBind
bindClick emit = do
  _ <- symbol "click"
  n <- FuncName <$> identifier
  v <- option emit (stateLow <|> stateHigh)
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

stateLow :: Parser State
stateLow = State <$> symbol "LOW"

stateHigh :: Parser State
stateHigh = State <$> symbol "HIGH"

stateNum :: Parser State
stateNum = do
  v <- show <$> decimal
  whiteSpace
  return $ State v

revertState :: State -> State
revertState (State "LOW") = State "HIGH"
revertState _             = State "LOW"

-- | Top level parser, parses Gpio.
gpio :: Parser Gpio
gpio = do
  reserved reservedNameGpio
  name <- identifier
  p <- pin
  state <- option (State "LOW") (stateLow <|> stateHigh <|> stateNum)
  open <- option (revertState state) (stateLow <|> stateHigh)
  b <- option NoBind (bindParser (revertState state))

  return Gpio
    { gpioName  = name
    , gpioPin   = p
    , gpioState = state
    , gpioOpen  = open
    , gpioClose = revertState open
    , gpioBind  = b
    }
