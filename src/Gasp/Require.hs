module Gasp.Require
  ( Require (..)
  ) where

newtype Require = Require FilePath
  deriving (Show, Eq)
