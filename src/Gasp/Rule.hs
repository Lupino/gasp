module Gasp.Rule
  ( Rule (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

data Rule = Rule
    { ruleCondition  :: !Text
    , ruleAction     :: !String
    , ruleElseAction :: !String
    } deriving (Show, Eq)

instance ToJSON Rule where
    toJSON rule = object
        [ "condition"   .= ruleCondition rule
        , "action"      .= ruleAction rule
        , "else_action" .= ruleElseAction rule
        , "has_else"    .= not (null $ ruleElseAction rule)
        ]
