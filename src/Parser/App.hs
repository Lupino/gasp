module Parser.App
    ( app
    ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import qualified Gasp.App           as App
import           Lexer
import           Parser.Common

-- | A type that describes supported app properties.
data AppProperty
    = Key !String
    | Token !String
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties = commaSep1 $ appPropertyKey <|> appPropertyToken

appPropertyKey :: Parser AppProperty
appPropertyKey = Key <$> gaspPropertyStringLiteral "key"

appPropertyToken :: Parser AppProperty
-- TODO(matija): 'fav.png' currently does not work because of '.'. Support it.
appPropertyToken = Token <$> gaspPropertyStringLiteral "token"

-- TODO(matija): unsafe, what if empty list?
getAppKey :: [AppProperty] -> String
getAppKey ps = head $ [t | Key t <- ps]

-- TODO(matija): unsafe, what if empty list?
getAppToken :: [AppProperty] -> String
getAppToken ps = head $ [t | Token t <- ps]

-- | Top level parser, parses App.
app :: Parser App.App
app = do
    (appName, appProps) <- gaspElementNameAndClosureContent reservedNameApp appProperties

    return App.App
        { App.appName = appName
        , App.appKey = getAppKey appProps
        , App.appToken = getAppToken appProps
        }
