module Gasp.Block
  ( Loop (..)
  , Setup (..)
  , Raw (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

data Loop = Loop
  { loopName :: String
  , loopCode :: Text
  } deriving (Show)

instance Eq Loop where
  x == y = loopName x == loopName y


instance ToJSON Loop where
  toJSON loop = object
    [ "code" .= loopCode loop
    , "name" .= loopName loop
    ]

data Setup = Setup
  { setupName :: String
  , setupCode :: Text
  } deriving (Show)


instance Eq Setup where
  x == y = setupName x == setupName y

instance ToJSON Setup where
  toJSON setup = object
    [ "code" .= setupCode setup
    , "name" .= setupName setup
    ]

data Raw = Raw
  { rawName :: String
  , rawCode :: Text
  } deriving (Show)


instance Eq Raw where
  x == y = rawName x == rawName y


instance ToJSON Raw where
  toJSON raw = object
    [ "code" .= rawCode raw
    , "name" .= rawName raw
    ]
