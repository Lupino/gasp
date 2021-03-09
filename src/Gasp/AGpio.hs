module Gasp.AGpio
    ( AGpio(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data AGpio = AGpio
    { agpioName :: !String -- Identifier
    , agpioPin  :: !String
    , agpioLink :: !String
    } deriving (Show, Eq)

instance ToJSON AGpio where
    toJSON agpio = object
        [ "name"     .= agpioName agpio
        , "pin"      .= agpioPin  agpio
        , "link"     .= agpioLink agpio
        , "has_link" .= not (null $ agpioLink agpio)
        ]
