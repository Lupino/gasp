module Gasp.Metric
    ( Metric(..)
    , getTotalMetricThresholdLength
    , setMetricThresholdLength
    , getMetricValueLength
    , getMetricThresholdRspLength
    , isFloatMetric
    , getMetricDataLength

    , MetricName (..)
    ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Gasp.Common

newtype MetricName = MetricName String
  deriving (Show, Eq)

instance ToJSON MetricName where
  toJSON (MetricName n) = toJSON n

data Metric = Metric
    { metricName         :: !MetricName -- Identifier
    , metricAddr         :: !Int
    , metricType         :: !DataType
    , metricMax          :: !Double
    , metricMin          :: !Double
    , metricMinThreshold :: !Double
    , metricMaxThreshold :: !Double
    , metricThreshold    :: !Double
    , metricPrec         :: !Int
    , metricAuto         :: !Bool
    , metricIdx          :: !Int
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
        , "is_float"        .= isFloatMetric metric
        , "uncheckmin"      .= (isUnsigned (metricType metric) && metricMin metric <= 0)
        , "onebyte"         .= (getMetricDataLength metric == 1)
        , "auto"            .= metricAuto metric
        , "is_long"         .= isLong (metricType metric)
        , "index"           .= metricIdx metric
        ]

metricNameLen :: Metric -> Int
metricNameLen Metric { metricName = MetricName n } = length n

-- {"method": "set_name_threshold", "data": vv.vv}
setMetricThresholdLength :: Metric -> Int
setMetricThresholdLength metric =
  38 + metricNameLen metric + getMetricThresholdValueLength metric

-- {"name_threshold": vv.vv}
getMetricThresholdRspLength :: Metric -> Int
getMetricThresholdRspLength metric =
  16 + metricNameLen metric + getMetricThresholdValueLength metric

getTotalMetricThresholdLength :: Int -> [Metric] -> Int
getTotalMetricThresholdLength =
  foldl $ \ v x -> v + getMetricThresholdRspLength x

getMetricValueLength :: Metric -> Int
getMetricValueLength metric = calcMetricWidth metric + 1 + metricPrec metric

getMetricThresholdValueLength :: Metric -> Int
getMetricThresholdValueLength metric = calcMetricThresholdWidth metric + 1 + metricPrec metric

isFloatMetric :: Metric -> Bool
isFloatMetric = isFloat . metricType

getMetricDataLength :: Metric -> Int
getMetricDataLength = dataLength . metricType
