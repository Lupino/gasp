module Gasp.Uart
  ( Uart (..)
  , UartName (..)
  , UartWriter (..)
  , UartReader (..)
  ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Function (FuncName)

newtype UartName = UartName String
  deriving (Show, Eq)


instance ToJSON UartName where
  toJSON (UartName n) = toJSON n

data Uart = Uart
  { uartName    :: !UartName
  , uartSpeed   :: !Int
  , uartWriters :: [UartWriter]
  , uartReaders :: [UartReader]
  } deriving (Show, Eq)

instance ToJSON Uart where
    toJSON uart = object
      [ "name"    .= uartName  uart
      , "speed"   .= uartSpeed uart
      , "readers" .= uartReaders uart
      , "writers" .= uartWriters uart
      , "wcount"  .= length (filter filterFunc $ uartWriters uart)
      ]
      where filterFunc :: UartWriter -> Bool
            filterFunc uw = uartWOn uw /= "false"

data UartWriter = UartWriter
  { uartWName :: String
  , uartWCmd  :: String
  , uartWId   :: Int
  , uartWOn   :: !String
  } deriving (Show, Eq)

instance ToJSON UartWriter where
    toJSON uw = object
        [ "wname"  .= uartWName uw
        , "bytes"  .= toHex (uartWCmd uw)
        , "index"  .= uartWId uw
        , "on"     .= uartWOn uw
        , "has_on" .= not (null $ uartWOn uw)
        , "auto"   .= (uartWOn uw /= "false")
        ]


toHex :: String -> [String]
toHex []       = []
toHex [_]      = error "wrong hex string"
toHex (x:y:xs) = [x, y]: toHex xs

data UartReader = UartReader
  { uartRId     :: Int
  , uartRFn     :: FuncName
  , uartRPFn    :: FuncName
  , uartRBufLen :: Int
  , uartROn     :: !String
  } deriving (Show, Eq)

instance ToJSON UartReader where
    toJSON ur = object
        [ "index"   .= uartRId ur
        , "reader"  .= uartRFn ur
        , "parser"  .= uartRPFn ur
        , "buf_len" .= uartRBufLen ur
        , "on"       .= uartROn ur
        , "has_on"   .= not (null $ uartROn ur)
        ]
