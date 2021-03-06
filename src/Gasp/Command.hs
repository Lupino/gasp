module Gasp.Command
    ( Command(..)
    , DocItem (..)
    , Doc (..)
    , getCmdRspLength
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Gasp.Flag  (Flag)

data DocItem = DocItem
  { itemDocs :: [String]
  , itemCmd  :: String
  } deriving (Show, Eq)


instance ToJSON DocItem where
  toJSON di = object
    [ "docs"    .= itemDocs di
    , "payload" .= itemCmd di
    , "has_doc" .= not (null $ itemDocs di)
    ]

data Doc = Doc
  { docName :: String
  , docCmd  :: DocItem
  , docRet  :: DocItem
  , docErr  :: DocItem
  } deriving (Show, Eq)

instance ToJSON Doc where
  toJSON doc = object
    [ "name"    .= docName doc
    , "command" .= docCmd doc
    , "return"  .= docRet doc
    , "error"   .= docErr doc
    ]

data Command = Command
  { cmdName :: !String -- Identifier
  , cmdFunc :: !String
  , cmdErrS :: !String
  , cmdDocS :: !Doc
  , cmdFlag :: !Flag
  } deriving (Show, Eq)

instance ToJSON Command where
    toJSON cmd = object
        [ "name"    .= cmdName cmd
        , "fn"      .= cmdFunc cmd
        , "flag"    .= cmdFlag cmd
        , "error"   .= cmdErrS cmd
        , "docs"    .= cmdDocS cmd
        ]

getCmdRspLength :: Command -> Int
getCmdRspLength Command {cmdErrS = err} = max okLen errLen
  where errLen = length err + 11
        okLen  = 22
