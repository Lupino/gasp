module Gasp.Import
  ( Import (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Import = Import String String
  deriving (Show)

instance Eq Import where
  (Import x _) == (Import y _) = x == y

instance ToJSON Import where
  toJSON (Import name url) = object
    [ "name" .= name
    , "url" .= url
    ]
