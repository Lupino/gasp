module Gasp.Command
    ( Command(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)
import qualified Data.Text  as T (length)
import           Gasp.Flag  (Flag)

data Command = Command
    { cmdName :: !String -- Identifier
    , cmdFunc :: !String
    , cmdErrS :: !String
    , cmdDocS :: !Text
    , cmdFlag :: !Flag
    } deriving (Show, Eq)

instance ToJSON Command where
    toJSON cmd = object
        [ "name"    .= cmdName cmd
        , "fn"      .= cmdFunc cmd
        , "flag"    .= cmdFlag cmd
        , "error"   .= cmdErrS cmd
        , "doc"     .= cmdDocS cmd
        , "has_doc" .= (T.length (cmdDocS cmd) > 0)
        ]
