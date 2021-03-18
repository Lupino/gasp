module Gasp.Uart
  ( Uart (..)
  , UartName (..)
  , UartWriter (..)
  , UartReader (..)
  , ModeBind (..)
  ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Attr     (AttrName)
import           Gasp.Function (FuncName)
import           Gasp.Gpio     (Pin)

newtype UartName = UartName String
  deriving (Show, Eq)


instance ToJSON UartName where
  toJSON (UartName n) = toJSON n


data ModeBind
  = LinkAttr AttrName
  | NoBind
  deriving (Show, Eq)

instance ToJSON ModeBind where
    toJSON (LinkAttr link) = object
        [ "link"       .= link
        , "is_link"    .= True
        , "is_no_bind" .= False
        ]
    toJSON NoBind = object
        [ "is_no_bind" .= True
        , "is_link"    .= False
        ]

data Uart = Uart
  { uartName    :: !UartName
  , uartTxPin   :: !Pin
  , uartRxPin   :: !Pin
  , uartSpeed   :: !Int
  , uartWriters :: [UartWriter]
  , uartReaders :: [UartReader]
  , uartBind    :: ModeBind
  } deriving (Show, Eq)

instance ToJSON Uart where
    toJSON uart = object
        [ "name"    .= uartName  uart
        , "tx"      .= uartTxPin uart
        , "rx"      .= uartRxPin uart
        , "speed"   .= uartSpeed uart
        , "readers" .= uartReaders uart
        , "writers" .= uartWriters uart
        , "wcount"  .= length (uartWriters uart)
        , "bind"    .= uartBind uart
        ]

data UartWriter = UartWriter
  { uartWName :: String
  , uartWCmd  :: String
  , uartWId   :: Int
  , uartWMode :: Int
  } deriving (Show, Eq)

instance ToJSON UartWriter where
    toJSON uw = object
        [ "wname" .= uartWName uw
        , "bytes" .= toHex (uartWCmd uw)
        , "index" .= uartWId uw
        , "mode"  .= uartWMode uw
        ]


toHex :: String -> [String]
toHex []       = []
toHex [_]      = error "wrong hex string"
toHex (x:y:xs) = (x:y:[]): toHex xs

data UartReader = UartReader
  { uartRName   :: String
  , uartRFn     :: FuncName
  , uartRPFn    :: FuncName
  , uartRBufLen :: Int
  } deriving (Show, Eq)

instance ToJSON UartReader where
    toJSON ur = object
        [ "rname"   .= uartRName ur
        , "reader"  .= uartRFn ur
        , "parser"  .= uartRPFn ur
        , "buf_len" .= uartRBufLen ur
        ]
