module Gasp.App
    ( App(..)
    , appContexLength
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))


data App = App
  { appName      :: !String -- Identifier
  , appKey       :: !String
  , appToken     :: !String
  , appAddr      :: !String
  , appStartAddr :: !Int
  , appCtrl      :: !Bool
  } deriving (Show, Eq)

instance ToJSON App where
    toJSON app = object
      [ "name"           .= appName app
      , "key"            .= key
      , "key_len"        .= keyLen
      , "key_hex_array"  .= hexArray (toHex key)
      , "token"          .= token
      , "token_len"      .= tokenLen
      , "token_hex_array".= hexArray (toHex token)
      , "start_addr"     .= appStartAddr app
      , "addr"           .= addr
      , "addr_len"       .= addrLen
      , "addr_hex_array" .= hexArray (toHex addr)
      , "context_len"    .= contextLen
      ]
      where key        = appKey app
            token      = appToken app
            keyLen     = length key `div` 2
            tokenLen   = length token `div` 2
            addr       = appAddr app
            addrLen    = length addr `div` 2
            contextLen = 4 + 1 + keyLen + 1 + tokenLen + 4 + 1 + addrLen + 1


toHex :: String -> [String]
toHex []       = []
toHex [_]      = error "wrong hex string"
toHex (x:y:xs) = (x:y:[]): toHex xs

hexArray :: [String] -> String
hexArray []     = []
hexArray [x]    = "0x" ++ x
hexArray (x:xs) = "0x" ++ x ++ ", " ++ hexArray xs

appContexLength :: App -> Int
appContexLength app = contextLen
  where key = appKey app
        token = appToken app
        keyLen = length key `div` 2
        tokenLen = length token `div` 2
        addr = appAddr app
        addrLen = length addr `div` 2
        contextLen = 4 + 1 + keyLen + 1 + tokenLen + 4 + 1 + addrLen + 1
