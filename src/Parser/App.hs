module Parser.App
    ( app
    ) where

import qualified Gasp.App           as App
import           Lexer
import           Parser.Common
import           Text.Parsec
import           Text.Parsec.String (Parser)

-- | A type that describes supported app properties.
data AppProperty
    = Key       !String
    | Token     !String
    | Addr      !String
    | StartAddr !Int
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties =
  commaSep1
  $ appPropertyKey
  <|> appPropertyToken
  <|> appPropertyStartAddr
  <|> try appPropertyAddr

appPropertyKey :: Parser AppProperty
appPropertyKey = Key <$> gaspPropertyStringLiteral "key"

appPropertyToken :: Parser AppProperty
appPropertyToken = Token <$> gaspPropertyStringLiteral "token"

appPropertyAddr :: Parser AppProperty
appPropertyAddr = Addr <$> gaspPropertyStringLiteral "addr"

appPropertyStartAddr :: Parser AppProperty
appPropertyStartAddr = StartAddr . fromIntegral <$> gaspPropertyInteger "start_addr"

-- | Top level parser, parses App.
app :: Parser App.App
app = do
    (appName, props) <- gaspElementNameAndClosureContent reservedNameApp appProperties

    return App.App
        { App.appName      = appName
        , App.appKey       = getFromList "" [t | Key t <- props]
        , App.appToken     = getFromList "" [t | Token t <- props]
        , App.appAddr      = getFromList "00000000" [t | Addr t <- props]
        , App.appStartAddr = getFromList 0 [t | StartAddr t <- props]
        }
