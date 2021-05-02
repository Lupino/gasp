module Gasp.Constant
  ( Constant (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Constant = Constant
    { constName  :: !String
    , constValue :: !String
    , constType  :: !String
    } deriving (Show, Eq)

instance ToJSON Constant where
    toJSON c = object
        [ "name"     .= constName c
        , "value"    .= constValue c
        , "type"     .= constType c
        , "has_type" .= not (null $ constType c)
        ]
