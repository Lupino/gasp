module Gasp.Every
    ( Every (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Every = Every
    { everyCore    :: !String
    , everyFunc    :: !String
    , everyDelayMs :: !String
    , everyOn      :: !String
    , everyIdx     :: !Int
    } deriving (Show, Eq)


instance ToJSON Every where
    toJSON every = object
        [ "core"     .= everyCore every
        , "fn"       .= everyFunc every
        , "delay_ms" .= everyDelayMs every
        , "on"       .= everyOn every
        , "has_on"   .= not (null $ everyOn every)
        , "id"       .= everyIdx every
        ]
