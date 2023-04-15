module Gasp.Uart
  ( Uart (..)
  , UartName (..)
  , UartWriter (..)
  , UartReader (..)
  , GenOrCmd (..)
  ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Function (FuncName)

newtype UartName = UartName String
  deriving (Show, Eq)


instance ToJSON UartName where
  toJSON (UartName n) = toJSON n

data Uart = Uart
  { uartCore    :: !String
  , uartName    :: !UartName
  , uartWriters :: [UartWriter]
  , uartReaders :: [UartReader]
  } deriving (Show, Eq)

instance ToJSON Uart where
    toJSON uart = object
      [ "core"    .= uartCore  uart
      , "name"    .= uartName  uart
      , "readers" .= uartReaders uart
      , "writers" .= uartWriters uart
      , "wcount"  .= length (filter filterFunc $ uartWriters uart)
      ]
      where filterFunc :: UartWriter -> Bool
            filterFunc uw = uartWOn uw /= "false"

data GenOrCmd = Cmd String | Gen FuncName Int
  deriving (Show, Eq)

instance ToJSON GenOrCmd where
    toJSON (Cmd cmd) = object
        [ "is_cmd" .= True
        , "is_gen" .= False
        , "bytes"  .= toHex cmd
        ]
    toJSON (Gen fn len) = object
        [ "is_cmd"  .= False
        , "is_gen"  .= True
        , "gen"     .= fn
        , "buf_len" .= len
        ]

data UartWriter = UartWriter
  { uartWName :: String
  , uartWAct  :: GenOrCmd
  , uartWId   :: Int
  , uartWOn   :: !String
  } deriving (Show, Eq)

instance ToJSON UartWriter where
    toJSON uw = object
        [ "wname"  .= uartWName uw
        , "action" .= uartWAct uw
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
  , uartRBufLen :: String
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
