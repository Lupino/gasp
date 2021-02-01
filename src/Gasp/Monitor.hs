module Gasp.Monitor
    ( Monitor (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Monitor = Monitor
    { monName    :: !String
    , monFunc    :: !String
    , monDelayMs :: !Integer
    } deriving (Show, Eq)


instance ToJSON Monitor where
    toJSON mon = object
        [ "name"     .= monName mon
        , "fn"       .= monFunc mon
        , "delay_ms" .= monDelayMs mon
        ]
