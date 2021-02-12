module Gasp.Rule
  ( Rule (..)
  ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Data.List   (nub)
import           Lexer       (identifier)
import           Text.Parsec (parse)

newtype MetricName = MetricName String deriving (Show, Eq)
instance ToJSON MetricName where
    toJSON (MetricName n) = object
      [ "name" .= n
      ]


data Rule = Rule
    { ruleCondition  :: !String
    , ruleAction     :: !String
    , ruleElseAction :: !String
    } deriving (Show, Eq)

instance ToJSON Rule where
    toJSON rule = object
        [ "condition"   .= ruleCondition rule
        , "action"      .= ruleAction rule
        , "else_action" .= ruleElseAction rule
        , "has_else"    .= not (null $ ruleElseAction rule)
        , "depends"     .= nub (getMetricNames rule)
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
