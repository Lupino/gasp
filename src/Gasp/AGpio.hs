module Gasp.AGpio
    ( AGpio(..)
    , AGpioBind (..)
    ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Gasp.Metric (MetricName)


data AGpioBind
  = LinkMetric MetricName
  | NoBind
  deriving (Show, Eq)

instance ToJSON AGpioBind where
    toJSON (LinkMetric link) = object
        [ "link"    .= link
        , "is_link" .= True
        ]
    toJSON NoBind = object
        [ "is_no_bind" .= True
        ]

data AGpio = AGpio
    { agpioName :: !String -- Identifier
    , agpioPin  :: !String
    , agpioBind :: !AGpioBind
    } deriving (Show, Eq)

instance ToJSON AGpio where
    toJSON agpio = object
        [ "name"     .= agpioName agpio
        , "pin"      .= agpioPin  agpio
        , "bind"     .= agpioBind agpio
        ]
