module Parser.Metric
    ( metric
    ) where

import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Gasp.Metric        as Metric
import           Lexer
import           Parser.Common

-- | A type that describes supported app properties.
data MetricProperty
    = Var          !String
    | Type         !String
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
  v <- case key of
         "prec"          -> Prec <$> integer
         "var"           -> Var <$> identifier
         "type"          -> Type <$> stringLiteral
         "threshold"     -> Threshold <$> float
         "min_threshold" -> MinThreshold <$> float
         "max_threshold" -> MaxThreshold <$> float
         "min"           -> Min <$> float
         "max"           -> Max <$> float
         _               -> fail $ "no such " ++ key
  return v

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
metricProperties :: Parser [MetricProperty]
metricProperties = commaSep1 cusL

getMetricVar :: String -> [MetricProperty] -> String
getMetricVar def ps = fromMaybe def . listToMaybe $ [t | Var t <- ps]

getMetricType :: String -> [MetricProperty] -> String
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

-- | Top level parser, parses Metric.
metric :: Parser Metric.Metric
metric = do
    (metricName, metricProps) <- gaspElementNameAndClosureContent reservedNameMetric metricProperties

    let maxv = getMetricMax 100 metricProps
        mint = maxv / 100
        maxt = maxv / 2

    return Metric.Metric
        { Metric.metricName         = metricName
        , Metric.metricVar          = getMetricVar metricName metricProps
        , Metric.metricType         = getMetricType "float" metricProps
        , Metric.metricMax          = maxv
        , Metric.metricMin          = getMetricMin 0 metricProps
        , Metric.metricMaxThreshold = getMetricMaxThreshold maxt metricProps
        , Metric.metricMinThreshold = getMetricMinThreshold mint metricProps
        , Metric.metricThreshold    = getMetricThreshold mint metricProps
        , Metric.metricPrec         = fromIntegral $ getMetricPrec 2 metricProps
        , Metric.metricAddr         = "0"
        }
