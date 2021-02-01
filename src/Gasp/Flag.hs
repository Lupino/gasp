module Gasp.Flag
    ( Flag (..)
    , defFlag
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Flag = Flag
    { flagJson   :: !Bool
    , flagFunc   :: !String
    , flagRetVal :: !Bool
    } deriving (Show, Eq)

defFlag :: Flag
defFlag = Flag
  { flagJson   = False
  , flagFunc   = ""
  , flagRetVal = False
  }

instance ToJSON Flag where
    toJSON flag = object
        [ "json"   .= flagJson flag
        , "retval" .= flagRetVal flag
        ]
