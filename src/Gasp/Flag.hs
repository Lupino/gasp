module Gasp.Flag
    ( Flag (..)
    , initFlag
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Flag = Flag
    { flagJson   :: !Bool
    , flagFunc   :: !String
    , flagRetval :: !Bool
    } deriving (Show)

instance Eq Flag where
  x == y = flagFunc x == flagFunc y

initFlag :: String -> Flag
initFlag func = Flag
  { flagJson   = False
  , flagFunc   = func
  , flagRetval = False
  }

instance ToJSON Flag where
    toJSON flag = object
        [ "json"   .= flagJson flag
        , "retval" .= flagRetval flag
        ]
