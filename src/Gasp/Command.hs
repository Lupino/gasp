module Gasp.Command
    ( Command(..)
    , DocItem (..)
    , Doc (..)
    , getCmdRspLength
    ) where


import           Data.Aeson           (ToJSON (..), Value, encode, object, (.=))
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (Text)
import qualified Data.Text            as T (lines, strip, unlines)
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Yaml            as Y (encode)
import           Gasp.Flag            (Flag)

data DocItem = DocItem
  { itemDocs :: [String]
  , itemCmd  :: Value
  } deriving (Show, Eq)


addSpace :: Text -> Text
addSpace = T.strip . T.unlines . map ("      "<>) . T.lines

instance ToJSON DocItem where
  toJSON di = object
    [ "docs"         .= itemDocs di
    , "payload"      .= decodeUtf8 (toStrict (encode (itemCmd di)))
    , "payload_yaml" .= addSpace (decodeUtf8 (Y.encode (itemCmd di)))
    , "has_doc"      .= not (null $ itemDocs di)
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
