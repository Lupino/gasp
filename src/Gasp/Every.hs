module Gasp.Every
    ( Every (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Every = Every
    { everyFunc    :: !String
    , everyDelayMs :: !Integer
    } deriving (Show, Eq)


instance ToJSON Every where
    toJSON every = object
        [ "fn"       .= everyFunc every
        , "delay_ms" .= everyDelayMs every
        ]
