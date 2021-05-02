module Gasp.Import
  ( Import (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Import = Import String String
  deriving (Show, Eq)


instance ToJSON Import where
  toJSON (Import name url) = object
    [ "name" .= name
    , "url" .= url
    ]
