module Parser.Gpio
    ( gpio
    ) where

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Gasp.Gpio
import           Lexer
import           Parser.Common
import           Text.Parsec.String (Parser)

-- | A type that describes supported app properties.
data GpioProperty
    = Pin     !String
    | Link    !String
    | Func    !String
    | Emit    !String
    | State   !String
    | Open    !String
    | Close   !String
    | Reverse !Bool
    deriving (Show, Eq)

-- | Parses gasp property along with the key, "key: value".
propParser :: Parser GpioProperty
propParser = do
  key <- identifier
  _ <- colon
  case key of
    "pin"     -> Pin <$> stringLiteral
    "link"    -> Link <$> identifier
    "fn"      -> Func <$> identifier
    "emit"    -> Emit <$> identifier
    "state"   -> State <$> identifier
    "open"    -> Open <$> identifier
    "close"   -> Close <$> identifier
    "reverse" -> Reverse <$> bool
    _         -> fail $ "no such " ++ key

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
gpioProperties :: Parser [GpioProperty]
gpioProperties = commaSep1 propParser

getGpioPin :: String -> [GpioProperty] -> String
getGpioPin def ps = fromMaybe def . listToMaybe $ [t | Pin t <- ps]

getGpioLink :: String -> [GpioProperty] -> String
getGpioLink def ps = fromMaybe def . listToMaybe $ [t | Link t <- ps]

getGpioFunc :: String -> [GpioProperty] -> String
getGpioFunc def ps = fromMaybe def . listToMaybe $ [t | Func t <- ps]

getGpioEmit :: String -> [GpioProperty] -> String
getGpioEmit def ps = fromMaybe def . listToMaybe $ [t | Emit t <- ps]

getGpioState :: String -> [GpioProperty] -> String
getGpioState def ps = fromMaybe def . listToMaybe $ [t | State t <- ps]

getGpioOpen :: String -> [GpioProperty] -> String
getGpioOpen def ps = fromMaybe def . listToMaybe $ [t | Open t <- ps]

getGpioClose :: String -> [GpioProperty] -> String
getGpioClose def ps = fromMaybe def . listToMaybe $ [t | Close t <- ps]

getGpioReverse :: Bool -> [GpioProperty] -> Bool
getGpioReverse def ps = fromMaybe def . listToMaybe $ [t | Reverse t <- ps]

-- | Top level parser, parses Gpio.
gpio :: Parser Gpio
gpio = do
    (name, gpioProps) <- gaspElementNameAndClosureContent reservedNameGpio gpioProperties

    return Gpio
        { gpioName    = name
        , gpioPin     = getGpioPin     "LED_BUILTIN" gpioProps
        , gpioLink    = getGpioLink    ""            gpioProps
        , gpioFunc    = getGpioFunc    ""            gpioProps
        , gpioEmit    = getGpioEmit    "HIGH"        gpioProps
        , gpioState   = getGpioState   "LOW"         gpioProps
        , gpioOpen    = getGpioOpen    "HIGH"        gpioProps
        , gpioClose   = getGpioClose   "LOW"         gpioProps
        , gpioReverse = getGpioReverse False         gpioProps
        }
