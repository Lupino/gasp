module Gasp.Command
    ( Command(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Gasp.Flag  (Flag)

data Command = Command
    { cmdName :: !String -- Identifier
    , cmdFunc :: !String
    , cmdErrS :: !String
    , cmdFlag :: !Flag
    } deriving (Show, Eq)

instance ToJSON Command where
    toJSON cmd = object
        [ "name"  .= cmdName cmd
        , "fn"    .= cmdFunc cmd
        , "flag"  .= cmdFlag cmd
        , "error" .= cmdErrS cmd
        ]
