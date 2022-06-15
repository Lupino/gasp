module Parser.Metric
    ( metric
    ) where

import           Gasp.Common        (DataType (..), maxValue, minValue)
import qualified Gasp.Metric        as Metric
import           Lexer
import           Parser.Common
import           Text.Parsec.String (Parser)

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


isAuto :: [MetricProperty] -> Bool
isAuto []                 = False
isAuto (Threshold _:_)    = True
isAuto (MaxThreshold _:_) = True
isAuto (MinThreshold _:_) = True
isAuto (_:xs)             = isAuto xs

-- | Top level parser, parses Metric.
metric :: Parser Metric.Metric
metric = do
    (metricName, props) <- gaspElementNameAndClosureContent reservedNameMetric metricProperties

    let tp = getFromList (DataType "float") [t | Type t <- props]
        tpMax = maxValue tp
        tpMin = minValue tp
        prec = fromIntegral $ getFromList 2 [t | Prec t <- props]
        maxv = min tpMax $ getFromList (maxValue tp) [t | Max t <- props]
        minv = max tpMin $ getFromList (minValue tp) [t | Min t <- props]
        maxt = (maxv - minv) / 2
        mint = max (10 ^^ (0-prec)) (maxt / 50)

    return Metric.Metric
        { Metric.metricName         = Metric.MetricName metricName
        , Metric.metricType         = tp
        , Metric.metricMax          = maxv
        , Metric.metricMin          = minv
        , Metric.metricMaxThreshold = min maxv $ getFromList maxt [t | MaxThreshold t <- props]
        , Metric.metricMinThreshold = max minv $ getFromList mint [t | MinThreshold t <- props]
        , Metric.metricThreshold    = getFromList mint [t | Threshold t <- props]
        , Metric.metricPrec         = prec
        , Metric.metricAddr         = 0
        , Metric.metricAuto         = isAuto props
        , Metric.metricIdx          = 0
        }
