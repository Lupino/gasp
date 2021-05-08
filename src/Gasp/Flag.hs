module Gasp.Flag
  ( Flag (..)
  ) where

import           Data.Text (Text)

data Flag = Flag
  { flagName  :: !Text
  , flagValue :: !Bool
  } deriving (Show)

instance Eq Flag where
  x == y = flagName x == flagName y
