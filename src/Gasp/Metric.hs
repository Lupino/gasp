module Gasp.Metric
    ( Metric(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Metric = Metric
    { metricName         :: !String -- Identifier
    , metricAddr         :: !String
    , metricVar          :: !String
    , metricType         :: !String
    , metricMax          :: !Double
    , metricMin          :: !Double
    , metricMinThreshold :: !Double
    , metricMaxThreshold :: !Double
    , metricThreshold    :: !Double
    , metricWidth        :: !Int
    , metricPrec         :: !Int
    } deriving (Show, Eq)

instance ToJSON Metric where
    toJSON metric = object
        [ "name"          .= metricName metric
        , "var"           .= metricVar  metric
        , "type"          .= metricType metric
        , "max"           .= metricMax metric
        , "min"           .= metricMin metric
        , "min_threshold" .= metricMinThreshold metric
        , "max_threshold" .= metricMaxThreshold metric
        , "threshold"     .= metricThreshold metric
        , "width"         .= metricWidth metric
        , "prec"          .= metricPrec metric
        , "addr"          .= metricAddr metric
        ]
