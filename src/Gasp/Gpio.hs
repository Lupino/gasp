module Gasp.Gpio
    ( Gpio(..)
    , State (..)
    , GpioBind (..)
    , Pin (..)
    ) where


import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Attr     (AttrName)
import           Gasp.Function (FuncName)

data Pin
  = PinName String
  | PinNum Integer
  deriving (Show, Eq)

instance ToJSON Pin where
  toJSON (PinName n) = toJSON n
  toJSON (PinNum n)  = toJSON n

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
        [ "link"       .= link
        , "reverse"    .= rev
        , "is_link"    .= True
        , "is_pwm"     .= False
        , "is_fn"      .= False
        , "is_no_bind" .= False
        ]
    toJSON (PwmAttr link) = object
        [ "link"       .= link
        , "is_pwm"     .= True
        , "is_link"    .= False
        , "is_fn"      .= False
        , "is_no_bind" .= False
        ]
    toJSON (CallFn fn emit) = object
        [ "fn"         .= fn
        , "emit"       .= emit
        , "is_fn"      .= True
        , "is_link"    .= False
        , "is_pwm"     .= False
        , "is_no_bind" .= False
        ]
    toJSON NoBind = object
        [ "is_no_bind" .= True
        , "is_link"    .= False
        , "is_pwm"     .= False
        , "is_fn"      .= False
        ]

data Gpio = Gpio
    { gpioName  :: !String -- Identifier
    , gpioPin   :: !Pin
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
