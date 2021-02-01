module Gasp.Telemetry
    ( Telemetry (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Gasp.Flag  (Flag)

data Telemetry = Telemetry
    { telemFunc :: !String
    , telemFlag :: !Flag
    } deriving (Show, Eq)


instance ToJSON Telemetry where
    toJSON cmd = object
        [ "fn" .= telemFunc cmd
        , "flag" .= telemFlag cmd
        ]
