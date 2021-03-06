module Gasp.AGpio
    ( AGpio(..)
    , AGpioBind (..)
    ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Gasp.Gpio   (Pin)
import           Gasp.Metric (MetricName)


data AGpioBind
  = LinkMetric MetricName
  | NoBind
  deriving (Show, Eq)

instance ToJSON AGpioBind where
    toJSON (LinkMetric link) = object
        [ "link"       .= link
        , "is_link"    .= True
        , "is_no_bind" .= False
        ]
    toJSON NoBind = object
        [ "is_no_bind" .= True
        , "is_link"    .= False
        ]

data AGpio = AGpio
    { agpioName :: !String -- Identifier
    , agpioPin  :: !Pin
    , agpioBind :: !AGpioBind
    } deriving (Show, Eq)

instance ToJSON AGpio where
    toJSON agpio = object
        [ "name" .= agpioName agpio
        , "pin"  .= agpioPin  agpio
        , "bind" .= agpioBind agpio
        ]
