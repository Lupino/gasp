module Gasp.Block
  ( Loop (..)
  , Setup (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

newtype Loop = Loop
  { loopCode :: Text
  } deriving (Show, Eq)


instance ToJSON Loop where
  toJSON loop = object
    [ "code" .= loopCode loop
    ]

newtype Setup = Setup
  { setupCode :: Text
  } deriving (Show, Eq)


instance ToJSON Setup where
  toJSON setup = object
    [ "code" .= setupCode setup
    ]
