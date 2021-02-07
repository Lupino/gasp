module Gasp.Metric
    ( Metric(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Metric = Metric
    { metricName         :: !String -- Identifier
    , metricAddr         :: !Int
    , metricType         :: !String
    , metricMax          :: !Double
    , metricMin          :: !Double
    , metricMinThreshold :: !Double
    , metricMaxThreshold :: !Double
    , metricThreshold    :: !Double
    , metricPrec         :: !Int
    } deriving (Show, Eq)

calcWitdh :: Double -> Int
calcWitdh v = length $ show (floor v :: Int)

instance ToJSON Metric where
    toJSON metric = object
        [ "name"            .= metricName metric
        , "type"            .= metricType metric
        , "max"             .= metricMax metric
        , "min"             .= metricMin metric
        , "min_threshold"   .= metricMinThreshold metric
        , "max_threshold"   .= metricMaxThreshold metric
        , "threshold"       .= metricThreshold metric
        , "threshold_width" .= calcWitdh (metricMaxThreshold metric)
        , "width"           .= calcWitdh (metricMax metric)
        , "prec"            .= metricPrec metric
        , "addr"            .= metricAddr metric
        ]
