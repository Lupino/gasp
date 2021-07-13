module Gasp.Block
  ( Loop (..)
  , Setup (..)
  , Raw (..)
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

newtype Raw = Raw
  { rawCode :: Text
  } deriving (Show, Eq)


instance ToJSON Raw where
  toJSON raw = object
    [ "code" .= rawCode raw
    ]
