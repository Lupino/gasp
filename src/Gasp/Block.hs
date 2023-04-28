module Gasp.Block
  ( Loop (..)
  , Setup (..)
  , Loop1 (..)
  , Setup1 (..)
  , Raw (..)
  , Data (..)
  , Tmpl (..)
  , Render (..)
  , Render1 (..)
  , Require (..)
  , Import (..)
  , Fd (..)

  , IfEq (..)
  , IfNeq (..)

  , Flag (..)
  , getFlag
  , defaultFlags
  ) where

import           Data.Aeson (ToJSON (..), Value, object, (.=))
import           Data.Text  (Text, pack, stripStart)

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
    [ "code" .= stripStart (loopCode loop)
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
    [ "code" .= stripStart (setupCode setup)
    , "name" .= setupName setup
    ]

data Loop1 = Loop1
  { loop1Name :: String
  , loop1Code :: Text
  } deriving (Show)

instance Eq Loop1 where
  x == y = loop1Name x == loop1Name y

instance Ord Loop1 where
  compare x y = compare (loop1Name x) (loop1Name y)


instance ToJSON Loop1 where
  toJSON loop1 = object
    [ "code" .= stripStart (loop1Code loop1)
    , "name" .= loop1Name loop1
    ]

data Setup1 = Setup1
  { setup1Name :: String
  , setup1Code :: Text
  } deriving (Show)


instance Eq Setup1 where
  x == y = setup1Name x == setup1Name y

instance Ord Setup1 where
  compare x y = compare (setup1Name x) (setup1Name y)

instance ToJSON Setup1 where
  toJSON setup1 = object
    [ "code" .= stripStart (setup1Code setup1)
    , "name" .= setup1Name setup1
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
    [ "code" .= stripStart (rawCode raw)
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

newtype Render = Render
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

newtype Require = Require FilePath
  deriving (Show, Eq)

data Import = Import String String
  deriving (Show)

instance Eq Import where
  (Import x _) == (Import y _) = x == y

instance ToJSON Import where
  toJSON (Import name url) = object
    [ "name" .= name
    , "url"  .= url
    ]

data Flag = Flag
  { flagName  :: !String
  , flagValue :: !Bool
  } deriving (Show)

instance Eq Flag where
  x == y = flagName x == flagName y

getFlag :: Bool -> [Flag] -> String -> Bool
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

data Fd = Fd
  { fdId   :: !Int
  , fdCall :: !String
  } deriving (Show)

instance Eq Fd where
  x == y = fdId x == fdId y


instance ToJSON Fd where
  toJSON fd = object
    [ "fd" .= fdId fd
    , "call" .= fdCall fd
    ]

data IfEq = IfEq
  { ifeqName :: String
  , ifeqCode :: Text
  } deriving (Show, Eq, Ord)

instance ToJSON IfEq where
  toJSON ifeq = object
    [ "code" .= stripStart (ifeqCode ifeq)
    , "name" .= ifeqName ifeq
    ]

data IfNeq = IfNeq
  { ifneqName :: String
  , ifneqCode :: Text
  } deriving (Show, Eq, Ord)

instance ToJSON IfNeq where
  toJSON ifneq = object
    [ "code" .= stripStart (ifneqCode ifneq)
    , "name" .= ifneqName ifneq
    ]
