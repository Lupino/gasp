module Gasp.Block
  ( Loop (..)
  , Setup (..)
  , Raw (..)
  , Data (..)
  , Tmpl (..)
  , Render (..)
  , Render1 (..)
  ) where

import           Data.Aeson (ToJSON (..), Value, object, (.=))
import           Data.Text  (Text)

data Loop = Loop
  { loopName :: String
  , loopCode :: Text
  } deriving (Show)

instance Eq Loop where
  x == y = loopName x == loopName y

instance Ord Loop where
  compare x y = compare (loopName x) (loopName y)


instance ToJSON Loop where
  toJSON loop = object
    [ "code" .= loopCode loop
    , "name" .= loopName loop
    ]

data Setup = Setup
  { setupName :: String
  , setupCode :: Text
  } deriving (Show)


instance Eq Setup where
  x == y = setupName x == setupName y

instance Ord Setup where
  compare x y = compare (setupName x) (setupName y)

instance ToJSON Setup where
  toJSON setup = object
    [ "code" .= setupCode setup
    , "name" .= setupName setup
    ]

data Raw = Raw
  { rawName :: String
  , rawCode :: Text
  } deriving (Show)


instance Eq Raw where
  x == y = rawName x == rawName y

instance Ord Raw where
  compare x y = compare (rawName x) (rawName y)


instance ToJSON Raw where
  toJSON raw = object
    [ "code" .= rawCode raw
    , "name" .= rawName raw
    ]

data Data = Data
  { dataName :: String
  , dataData :: Value
  } deriving (Show)


instance Eq Data where
  x == y = dataName x == dataName y


instance Ord Data where
  compare x y = compare (dataName x) (dataName y)

instance ToJSON Data where
  toJSON dat = object
    [ "data" .= dataData dat
    , "name" .= dataName dat
    ]

data Render = Render
  { rdName :: String
  } deriving (Show)

instance Eq Render where
  x == y = rdName x == rdName y

data Render1 = Render1
  { rd1Name :: String
  , rd1Data :: Value
  } deriving (Show)

instance Eq Render1 where
  x == y = rd1Name x == rd1Name y

data Tmpl = Tmpl
  { tmplName :: String
  , tmplData :: Text
  } deriving (Show)

instance Eq Tmpl where
  x == y = tmplName x == tmplName y
