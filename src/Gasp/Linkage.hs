module Gasp.Linkage
  ( Linkage (..)
  , linkageDataLen
  , linkageRspLength
  , setLinkageLength
  ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Function (FuncName)


data Linkage = Linkage
  { linkageName :: !String -- Identifier
  , linkageFnG  :: !FuncName
  , linkageFn0  :: !FuncName
  , linkageFn1  :: !FuncName
  , linkageAddr :: Int
  } deriving (Show, Eq)

instance ToJSON Linkage where
    toJSON linkage = object
      [ "name"      .= linkageName linkage
      , "get"       .= linkageFnG linkage
      , "open"      .= linkageFn0 linkage
      , "close"     .= linkageFn1 linkage
      , "addr_type" .= addr0
      , "addr_mode" .= addr1
      , "addr_min"  .= addr2
      , "addr_max"  .= addr3
      ]
      where addr0 = linkageAddr linkage
            addr1 = addr0 + 1
            addr2 = addr1 + 1
            addr3 = addr2 + 4
            -- type uint8   1
            -- mode uint8   1
            -- min  float   4
            -- max  float   4

linkageDataLen :: Int
linkageDataLen = 1 + 1 + 4 + 4

linkageNameLen :: Linkage -> Int
linkageNameLen = length . linkageName

-- {"type": 32, "mode": 4, "min": -10000.0000, "max": 10000.0000}
linkageRspLength :: Int
linkageRspLength = 70

--           type,mode,min,max
-- {"data": "32,4,-10000.00,10000.00", "method": "set_linkage", "name": "vv"}
setLinkageLength :: Linkage -> Int
setLinkageLength = (80 +) . linkageNameLen
