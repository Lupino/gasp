module Parser.Metric
    ( metric
    ) where

import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Gasp.Common        (DataType (..), maxValue, minValue)
import qualified Gasp.Metric        as Metric
import           Lexer
import           Parser.Common

-- | A type that describes supported app properties.
data MetricProperty
    = Type         !DataType
    | Max          !Double
    | Min          !Double
    | MinThreshold !Double
    | MaxThreshold !Double
    | Threshold    !Double
    | Prec         !Integer
    deriving (Show, Eq)

-- | Parses gasp property along with the key, "key: value".
cusL :: Parser MetricProperty
cusL = do
  key <- identifier
  _ <- colon
  case key of
    "prec"          -> Prec <$> integer
    "type"          -> Type <$> dataType
    "threshold"     -> Threshold <$> float
    "min_threshold" -> MinThreshold <$> float
    "max_threshold" -> MaxThreshold <$> float
    "min"           -> Min <$> float
    "max"           -> Max <$> float
    _               -> fail $ "no such " ++ key

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
metricProperties :: Parser [MetricProperty]
metricProperties = commaSep1 cusL

getMetricType :: DataType -> [MetricProperty] -> DataType
getMetricType def ps = fromMaybe def . listToMaybe $ [t | Type t <- ps]

getMetricMax :: Double -> [MetricProperty] -> Double
getMetricMax def ps = fromMaybe def . listToMaybe $ [t | Max t <- ps]

getMetricMin :: Double -> [MetricProperty] -> Double
getMetricMin def ps = fromMaybe def . listToMaybe $ [t | Min t <- ps]

getMetricMinThreshold :: Double -> [MetricProperty] -> Double
getMetricMinThreshold def ps = fromMaybe def . listToMaybe $ [t | MinThreshold t <- ps]

getMetricMaxThreshold :: Double -> [MetricProperty] -> Double
getMetricMaxThreshold def ps = fromMaybe def . listToMaybe $ [t | MaxThreshold t <- ps]

getMetricThreshold :: Double -> [MetricProperty] -> Double
getMetricThreshold def ps = fromMaybe def . listToMaybe $ [t | Threshold t <- ps]

getMetricPrec :: Integer -> [MetricProperty] -> Integer
getMetricPrec def ps = fromMaybe def . listToMaybe $ [t | Prec t <- ps]


isAuto :: [MetricProperty] -> Bool
isAuto []                 = False
isAuto (Threshold _:_)    = True
isAuto (MaxThreshold _:_) = True
isAuto (MinThreshold _:_) = True
isAuto (_:xs)             = isAuto xs

-- | Top level parser, parses Metric.
metric :: Parser Metric.Metric
metric = do
    (metricName, metricProps) <- gaspElementNameAndClosureContent reservedNameMetric metricProperties

    let tp = getMetricType (DataType "float") metricProps
        maxv = getMetricMax (maxValue tp) metricProps
        minv = getMetricMin (minValue tp) metricProps
        maxt = (maxv - minv) / 2
        mint = maxt / 50

    return Metric.Metric
        { Metric.metricName         = Metric.MetricName metricName
        , Metric.metricType         = tp
        , Metric.metricMax          = maxv
        , Metric.metricMin          = minv
        , Metric.metricMaxThreshold = getMetricMaxThreshold maxt metricProps
        , Metric.metricMinThreshold = getMetricMinThreshold mint metricProps
        , Metric.metricThreshold    = getMetricThreshold mint metricProps
        , Metric.metricPrec         = fromIntegral $ getMetricPrec 2 metricProps
        , Metric.metricAddr         = 0
        , Metric.metricAuto         = isAuto metricProps
        }
