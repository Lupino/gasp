module Gasp.Constant
  ( Constant (..)
  , splitConstant
  ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Data.List   (partition)
import           Data.Text   (Text)
import qualified Data.Text   as T (pack, takeWhile)
import           Gasp.Common (GetCode (..), GetName (..))

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

instance GetName Constant where
  getName = T.takeWhile (/='[') . T.takeWhile (/='(') . T.pack . constName

instance GetCode Constant where
  getCode _ = T.pack ""


splitConstant :: [Constant] -> ([Constant], [Constant])
splitConstant = partition (null . constType)
