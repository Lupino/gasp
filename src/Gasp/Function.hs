module Gasp.Function
    ( Function (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)
import           Gasp.Flag  (Flag)

data Function = Function
    { funcName :: !String
    , funcCode :: !Text
    , funcFlag :: !Flag
    } deriving (Show, Eq)

instance ToJSON Function where
    toJSON func = object
        [ "name" .= funcName func
        , "code" .= funcCode func
        , "flag" .= funcFlag func
        ]
