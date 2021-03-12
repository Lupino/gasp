module Gasp.Gpio
    ( Gpio(..)
    , State (..)
    , GpioBind (..)
    , isInput
    ) where


import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Attr     (AttrName)
import           Gasp.Function (FuncName)

newtype State = State String
  deriving (Show, Eq)

instance ToJSON State where
  toJSON (State n) = toJSON n

data GpioBind
  = LinkAttr AttrName Bool
  | CallFn FuncName State
  | PwmAttr AttrName
  | NoBind
  deriving (Show, Eq)

instance ToJSON GpioBind where
    toJSON (LinkAttr link rev) = object
        [ "link"    .= link
        , "reverse" .= rev
        , "is_link" .= True
        ]
    toJSON (PwmAttr link) = object
        [ "link"    .= link
        , "is_pwm" .= True
        ]
    toJSON (CallFn fn emit) = object
        [ "fn"    .= fn
        , "emit"  .= emit
        , "is_fn" .= True
        ]
    toJSON NoBind = object
        [ "is_no_bind" .= True
        ]

isInput :: GpioBind -> Bool
isInput (LinkAttr _ _) = False
isInput (CallFn _ _)   = True
isInput (PwmAttr _)    = False
isInput NoBind         = False

data Gpio = Gpio
    { gpioName  :: !String -- Identifier
    , gpioPin   :: !String
    , gpioBind  :: !GpioBind
    , gpioState :: !State
    , gpioOpen  :: !State
    , gpioClose :: !State
    } deriving (Show, Eq)

instance ToJSON Gpio where
    toJSON gpio = object
        [ "name"  .= gpioName  gpio
        , "pin"   .= gpioPin   gpio
        , "bind"  .= gpioBind  gpio
        , "state" .= gpioState gpio
        , "open"  .= gpioOpen  gpio
        , "close" .= gpioClose gpio
        ]
