module Gasp.Init
    ( Init (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

newtype Init = Init
  { initCode :: Text
  } deriving (Show, Eq)


instance ToJSON Init where
    toJSON d = object
      [ "code" .= initCode d
      ]
