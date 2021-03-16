module Gasp.Every
    ( Every (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Every = Every
    { everyFunc    :: !String
    , everyDelayMs :: !Integer
    , everyOn      :: !String
    } deriving (Show, Eq)


instance ToJSON Every where
    toJSON every = object
        [ "fn"       .= everyFunc every
        , "delay_ms" .= everyDelayMs every
        , "on"       .= everyOn every
        , "has_on"   .= not (null $ everyOn every)
        ]
