module Gasp.Bin
    ( Bin (..)
    , BinName (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

newtype BinName = BinName String
  deriving (Show, Eq)

instance ToJSON BinName where
  toJSON (BinName n) = toJSON n

data Bin = Bin
  { binName :: !BinName -- Identifier
  , binType :: !String
  , binLen  :: !Int
  , binAddr :: !Int
  , binSize :: !Int
  } deriving (Show)

instance ToJSON Bin where
  toJSON bin = object
    [ "name" .= binName bin
    , "type" .= binType bin
    , "addr" .= binAddr bin
    , "size" .= binSize bin
    , "only" .= (binSize bin == 1)
    ]

instance Eq Bin where
  c0 == c1 = binName c0 == binName c1
