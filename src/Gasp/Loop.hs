module Gasp.Loop
    ( Loop (..)
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
