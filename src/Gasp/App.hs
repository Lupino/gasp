module Gasp.App
    ( App (..)
    , appContexLen
    , appTokenLen
    , startAddr
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))


data App = App
  { appName      :: !String -- Identifier
  , appKey       :: !String
  , appToken     :: !String
  , appAddr      :: !String
  , appStartAddr :: !Int
  } deriving (Show, Eq)

instance ToJSON App where
    toJSON app = object
      [ "name"           .= appName app
      , "key"            .= key
      , "key_len"        .= appKeyLen app
      , "key_hex_array"  .= hexArray (toHex key)
      , "token"          .= token
      , "token_addr"     .= appStartAddr app
      , "token_len"      .= appTokenLen app
      , "token_hex_array".= hexArray (toHex token)
      , "addr_addr"      .= (appStartAddr app + appTokenLen app)
      , "addr"           .= addr
      , "addr_hex_array" .= hexArray (toHex addr)
      ]
      where key        = appKey app
            token      = appToken app
            addr       = appAddr app


toHex :: String -> [String]
toHex []       = []
toHex [_]      = error "wrong hex string"
toHex (x:y:xs) = [x, y] : toHex xs

hexArray :: [String] -> String
hexArray []     = []
hexArray [x]    = "0x" ++ x
hexArray (x:xs) = "0x" ++ x ++ ", " ++ hexArray xs

hexLength :: String -> Int
hexLength = (`div` 2) . length

appTokenLen :: App -> Int
appTokenLen = hexLength . appToken

appKeyLen :: App -> Int
appKeyLen = hexLength . appKey

appAddrLen :: App -> Int
appAddrLen = hexLength . appAddr

magicLen :: Int
magicLen = 4

appContexLen :: App -> Int
appContexLen app =
  magicLen
  + 1 + appKeyLen app
  + 1 + appTokenLen app

startAddr :: Bool -> App -> Int
startAddr True app  = appStartAddr app + appTokenLen app + appAddrLen app
startAddr False app = appStartAddr app
