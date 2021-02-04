module Gasp.Gpio
    ( Gpio(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Gpio = Gpio
    { gpioName  :: !String -- Identifier
    , gpioPin   :: !String
    , gpioLink  :: !String
    , gpioFunc  :: !String
    , gpioEmit  :: !String
    , gpioState :: !String
    } deriving (Show, Eq)

instance ToJSON Gpio where
    toJSON gpio = object
        [ "name"     .= gpioName gpio
        , "pin"      .= gpioPin  gpio
        , "fn"       .= gpioFunc gpio
        , "link"     .= gpioLink gpio
        , "emit"     .= gpioEmit gpio
        , "state"    .= gpioState gpio
        , "has_link" .= not (null $ gpioLink gpio)
        , "has_fn"   .= not (null $ gpioFunc gpio)
        ]
