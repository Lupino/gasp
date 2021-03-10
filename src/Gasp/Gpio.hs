module Gasp.Gpio
    ( Gpio(..)
    , FuncName (..)
    , AttrName (..)
    , State (..)
    , GpioBind (..)
    , isInput
    ) where


import           Data.Aeson (ToJSON (..), object, (.=))

newtype AttrName = AttrName String
  deriving (Show, Eq)

newtype FuncName = FuncName String
  deriving (Show, Eq)

newtype State = State {unState :: String}
  deriving (Show, Eq)

data GpioBind
  = LinkAttr AttrName Bool
  | CallFn FuncName State
  | PwmAttr AttrName
  | NoBind
  deriving (Show, Eq)

instance ToJSON GpioBind where
    toJSON (LinkAttr (AttrName link) rev) = object
        [ "link"    .= link
        , "reverse" .= rev
        , "is_link" .= True
        ]
    toJSON (PwmAttr (AttrName link)) = object
        [ "link"    .= link
        , "is_pwm" .= True
        ]
    toJSON (CallFn (FuncName fn) (State emit)) = object
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
        , "state" .= unState (gpioState gpio)
        , "open"  .= unState (gpioOpen  gpio)
        , "close" .= unState (gpioClose gpio)
        ]
