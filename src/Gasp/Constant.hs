module Gasp.Constant
  ( Constant (..)
  , combineConstant
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Constant = Constant
  { constName  :: !String
  , constValue :: !String
  , constType  :: !String
  } deriving (Show)

instance Eq Constant where
  c0 == c1 = constName c0 == constName c1

instance ToJSON Constant where
    toJSON c = object
      [ "name"      .= constName c
      , "value"     .= constValue c
      , "has_value" .= not (null $ constValue c)
      , "type"      .= constType c
      , "has_type"  .= not (null $ constType c)
      ]


combineConstant :: [Constant] -> [Constant] -> [Constant]
combineConstant combined []  = combined
combineConstant combined (x:xs)
  | x `elem` combined = combineConstant combined xs
  | otherwise = combineConstant (combined ++ [x]) xs
