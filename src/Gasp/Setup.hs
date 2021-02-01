module Gasp.Setup
    ( Setup (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

newtype Setup = Setup
  { setupCode :: Text
  } deriving (Show, Eq)


instance ToJSON Setup where
    toJSON setup = object
        [ "code" .= setupCode setup
        ]
