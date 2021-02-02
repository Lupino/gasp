module Gasp.Attr
    ( Attr(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Attr = Attr
    { attrName  :: !String -- Identifier
    , attrAddr  :: !Int
    , attrVar   :: !String
    , attrMax   :: !Double
    , attrMin   :: !Double
    , attrType  :: !String
    , attrDef   :: !Double
    , attrScale :: !Double
    } deriving (Show, Eq)

instance ToJSON Attr where
    toJSON attr = object
        [ "name"       .= attrName  attr
        , "addr"       .= attrAddr  attr
        , "var"        .= attrVar   attr
        , "max"        .= attrMax   attr
        , "min"        .= attrMin   attr
        , "scaled_max" .= (attrMax  attr * attrScale attr)
        , "scaled_min" .= (attrMin  attr * attrScale attr)
        , "scale"      .= attrScale attr
        , "type"       .= attrType  attr
        , "default"    .= (attrDef  attr * attrScale attr)
        ]
