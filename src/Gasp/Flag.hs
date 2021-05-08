module Gasp.Flag
  ( Flag (..)
  , getFlag
  , defaultFlags
  ) where

import           Data.Text (Text)

data Flag = Flag
  { flagName  :: !Text
  , flagValue :: !Bool
  } deriving (Show)

instance Eq Flag where
  x == y = flagName x == flagName y

getFlag :: Bool -> [Flag] -> Text -> Bool
getFlag def [] _ = def
getFlag def (x:xs) n
  | n == flagName x = flagValue x
  | otherwise       = getFlag def xs n

defaultFlags :: [Flag]
defaultFlags =
  [ Flag "has_debug" False
  , Flag "ctrl_mode" False
  , Flag "auto_retry" True
  , Flag "low_memory" False
  ]
