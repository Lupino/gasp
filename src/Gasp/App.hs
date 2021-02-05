module Gasp.App
    ( App(..)
    , emptyApp
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))


data App = App
    { appName  :: !String -- Identifier
    , appKey   :: !String
    , appToken :: !String
    } deriving (Show, Eq)

instance ToJSON App where
    toJSON app = object
        [ "name" .= appName app
        , "key" .= appKey app
        , "token" .= appToken app
        ]

emptyApp :: App
emptyApp = App
  { appName  = "empty"
  , appKey   = "empty"
  , appToken = "empty"
  }
