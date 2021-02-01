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
    | Max          !String
    | Min          !String
    | MinThreshold !String
    | MaxThreshold !String
    | Threshold    !String
    | Width        !String
    | Prec         !String
    deriving (Show, Eq)

-- | Parses gasp property along with the key, "key: value".
cusL :: Parser (String, String)
cusL = do
  key <- identifier
  _ <- colon
  v <- case key of
         "width" -> integerString
         "prec"  -> integerString
         "var"   -> identifier
         "type"  -> stringLiteral
         _       -> floatString
  return (key, v)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
metricProperties :: Parser [MetricProperty]
metricProperties =
  map toMetricProperty <$> commaSep1 cusL


toMetricProperty :: (String, String) -> MetricProperty
toMetricProperty ("var", v)           = Var v
toMetricProperty ("type", v)          = Type v
toMetricProperty ("max", v)           = Max v
toMetricProperty ("min", v)           = Min v
toMetricProperty ("min_threshold", v) = MinThreshold v
toMetricProperty ("max_threshold", v) = MaxThreshold v
toMetricProperty ("threshold", v)     = Threshold v
toMetricProperty ("width", v)         = Width v
toMetricProperty ("prec", v)          = Prec v
toMetricProperty  (k, v)              = error $ "not such " ++ k ++ ": " ++ v

getMetricVar :: String -> [MetricProperty] -> String
getMetricVar def ps = fromMaybe def . listToMaybe $ [t | Var t <- ps]

getMetricType :: String -> [MetricProperty] -> String
getMetricType def ps = fromMaybe def . listToMaybe $ [t | Type t <- ps]

getMetricMax :: String -> [MetricProperty] -> String
getMetricMax def ps = fromMaybe def . listToMaybe $ [t | Max t <- ps]

getMetricMin :: String -> [MetricProperty] -> String
getMetricMin def ps = fromMaybe def . listToMaybe $ [t | Min t <- ps]

getMetricMinThreshold :: String -> [MetricProperty] -> String
getMetricMinThreshold def ps = fromMaybe def . listToMaybe $ [t | MinThreshold t <- ps]

getMetricMaxThreshold :: String -> [MetricProperty] -> String
getMetricMaxThreshold def ps = fromMaybe def . listToMaybe $ [t | MaxThreshold t <- ps]

getMetricThreshold :: String -> [MetricProperty] -> String
getMetricThreshold def ps = fromMaybe def . listToMaybe $ [t | Threshold t <- ps]

getMetricWidth :: String -> [MetricProperty] -> String
getMetricWidth def ps = fromMaybe def . listToMaybe $ [t | Width t <- ps]

getMetricPrec :: String -> [MetricProperty] -> String
getMetricPrec def ps = fromMaybe def . listToMaybe $ [t | Prec t <- ps]

-- | Top level parser, parses Metric.
metric :: Parser Metric.Metric
metric = do
    (metricName, metricProps) <- gaspElementNameAndClosureContent reservedNameMetric metricProperties

    let maxv = read (getMetricMax "100" metricProps) :: Float
        mint = show (maxv / 100)
        maxt = show (maxv / 2)

    return Metric.Metric
        { Metric.metricName         = metricName
        , Metric.metricVar          = getMetricVar metricName metricProps
        , Metric.metricType         = getMetricType "float" metricProps
        , Metric.metricMax          = getMetricMax "100" metricProps
        , Metric.metricMin          = getMetricMin "0" metricProps
        , Metric.metricMaxThreshold = getMetricMaxThreshold maxt metricProps
        , Metric.metricMinThreshold = getMetricMinThreshold mint metricProps
        , Metric.metricThreshold    = getMetricThreshold mint metricProps
        , Metric.metricWidth        = getMetricWidth "8" metricProps
        , Metric.metricPrec         = getMetricPrec "2" metricProps
        , Metric.metricAddr         = "0"
        }
