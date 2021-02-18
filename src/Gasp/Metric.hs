module Gasp.Metric
    ( Metric(..)
    , getTotalMetricThresholdLength
    , setMetricThresholdLength
    , getMetricValueLength
    , getMetricThresholdRspLength
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Gasp.Attr  (calcWidth)

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

calcMetricWidth :: Metric -> Int
calcMetricWidth metric = calcWidth (metricMin metric) (metricMax metric)

calcMetricThresholdWidth :: Metric -> Int
calcMetricThresholdWidth metric =
  calcWidth (metricMinThreshold metric) (metricMaxThreshold metric)

instance ToJSON Metric where
    toJSON metric = object
        [ "name"            .= metricName metric
        , "type"            .= metricType metric
        , "max"             .= metricMax metric
        , "min"             .= metricMin metric
        , "min_threshold"   .= metricMinThreshold metric
        , "max_threshold"   .= metricMaxThreshold metric
        , "threshold"       .= metricThreshold metric
        , "threshold_width" .= calcMetricThresholdWidth metric
        , "width"           .= calcMetricWidth metric
        , "prec"            .= metricPrec metric
        , "addr"            .= metricAddr metric
        ]

-- {"method": "set_name_threshold", "data": vv.vv}
setMetricThresholdLength :: Metric -> Int
setMetricThresholdLength metric =
  38 + length (metricName metric) + getMetricThresholdValueLength metric

-- {"name_threshold": vv.vv}
getMetricThresholdRspLength :: Metric -> Int
getMetricThresholdRspLength metric =
  16 + length (metricName metric) + getMetricThresholdValueLength metric

getTotalMetricThresholdLength :: Int -> [Metric] -> Int
getTotalMetricThresholdLength v [] = v
getTotalMetricThresholdLength v (x:xs) = getTotalMetricThresholdLength (v + getMetricThresholdRspLength x) xs

getMetricValueLength :: Metric -> Int
getMetricValueLength metric = calcMetricWidth metric + 1 + metricPrec metric

getMetricThresholdValueLength :: Metric -> Int
getMetricThresholdValueLength metric = calcMetricThresholdWidth metric + 1 + metricPrec metric
