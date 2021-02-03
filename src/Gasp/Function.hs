module Gasp.Function
    ( Function (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)
import qualified Data.Text  as T (length, lines, strip, words)
import           Gasp.Flag  (Flag)

data Function = Function
    { funcName :: !String
    , funcCode :: !Text
    , funcFlag :: !Flag
    } deriving (Show, Eq)

instance ToJSON Function where
    toJSON func = object
        [ "name"   .= funcName func
        , "code"   .= funcCode func
        , "flag"   .= funcFlag func
        , "return" .= lastReturn (funcCode func)
        ]

lastReturn :: Text -> Bool
lastReturn = go . T.lines
  where go :: [Text] -> Bool
        go [] = False
        go xs | T.length lstv > 0 = go1 (init xs) (T.words lstv)
              | otherwise = go $ init xs
          where lstv = T.strip $ last xs

        go1 :: [Text] -> [Text] -> Bool
        go1 _ ("return":_) = True
        go1 xs ("//":_)    = go xs
        go1 _ _            = False
