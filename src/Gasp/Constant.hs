module Gasp.Constant
  ( Constant (..)
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Constant = Constant
  { constName  :: !String
  , constValue :: !String
  , constArgv  :: !String
  , constType  :: !String
  } deriving (Show, Eq)

instance ToJSON Constant where
    toJSON c = object
      [ "name"      .= constName c
      , "value"     .= constValue c
      , "argv"      .= fixedargv
      , "has_value" .= not (null $ constValue c)
      , "type"      .= constType c
      , "has_type"  .= not (null $ constType c)
      ]

      where argv = constArgv c
            fixedargv = if null argv then "" else "(" ++ argv ++ ")"
