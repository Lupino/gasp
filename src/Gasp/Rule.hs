module Gasp.Rule
  ( Rule (..)
  ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Data.List   (nub)
import           Lexer       (identifier)
import           Text.Parsec (parse)

import           Gasp.Metric (MetricName (..))


data Rule = Rule
    { ruleCondition   :: !String
    , ruleOnCondition :: !String
    , ruleAction      :: !String
    , ruleLater       :: !String
    , ruleElseAction  :: !String
    , ruleElseLater   :: !String
    , ruleIndex       :: !Int
    } deriving (Show, Eq)

instance ToJSON Rule where
    toJSON rule = object
        [ "condition"      .= ruleCondition rule
        , "on_condition"   .= ruleOnCondition rule
        , "has_on"         .= not (null $ ruleOnCondition rule)
        , "action"         .= ruleAction rule
        , "later"          .= ruleLater rule
        , "has_later"      .= not (null $ ruleLater rule)
        , "else_action"    .= ruleElseAction rule
        , "else_later"     .= ruleElseLater rule
        , "has_else"       .= not (null $ ruleElseAction rule)
        , "has_else_later" .= not (null $ ruleElseLater rule)
        , "depends"        .= nub (getMetricNames rule)
        , "id"             .= ruleIndex rule
        ]


getMetricNames :: Rule -> [MetricName]
getMetricNames = getIdentifiers . ruleCondition

getIdentifiers :: String -> [MetricName]
getIdentifiers "" = []
getIdentifiers s =
  case parse identifier "" s of
    Left _ -> getIdentifiers $ drop 1 s
    Right ss ->
      case take 7 ss of
        "metric_" -> MetricName (drop 7 ss) : getIdentifiers (drop (length ss) s)
        _         -> getIdentifiers (drop (length ss) s)
