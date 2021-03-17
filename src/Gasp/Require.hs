module Gasp.Require
  ( Require (..)
  ) where

data Require = Require FilePath
  deriving (Show, Eq)
