module Parser.App
    ( app
    ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Gasp.App           as App
import           Lexer
import           Parser.Common

-- | A type that describes supported app properties.
data AppProperty
    = Key       !String
    | Token     !String
    | Addr      !String
    | StartAddr !Int
    | Ctrl      !Bool
    | Retry     !Bool
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties =
  commaSep1
  $ appPropertyKey
  <|> appPropertyToken
  <|> appPropertyStartAddr
  <|> appPropertyCtrl
  <|> try appPropertyAddr
  <|> try appPropertyRetry

appPropertyKey :: Parser AppProperty
appPropertyKey = Key <$> gaspPropertyStringLiteral "key"

appPropertyToken :: Parser AppProperty
appPropertyToken = Token <$> gaspPropertyStringLiteral "token"

appPropertyAddr :: Parser AppProperty
appPropertyAddr = Addr <$> gaspPropertyStringLiteral "addr"

appPropertyStartAddr :: Parser AppProperty
appPropertyStartAddr = StartAddr . fromIntegral <$> gaspPropertyInteger "start_addr"

appPropertyCtrl :: Parser AppProperty
appPropertyCtrl = Ctrl <$> gaspPropertyBool "ctrl_mode"

appPropertyRetry :: Parser AppProperty
appPropertyRetry = Retry <$> gaspPropertyBool "auto_retry"

getAppKey :: [AppProperty] -> String
getAppKey ps = head $ [t | Key t <- ps]

getAppToken :: [AppProperty] -> String
getAppToken ps = fromMaybe "" $ listToMaybe $ [t | Token t <- ps]

getAppAddr :: [AppProperty] -> String
getAppAddr ps = fromMaybe "00000000" $ listToMaybe $ [t | Addr t <- ps]

getAppStartAddr :: [AppProperty] -> Int
getAppStartAddr ps = fromMaybe 0 $ listToMaybe $ [t | StartAddr t <- ps]

getAppCtrl :: [AppProperty] -> Bool
getAppCtrl ps = fromMaybe False $ listToMaybe $ [t | Ctrl t <- ps]

getAppRetry :: [AppProperty] -> Bool
getAppRetry ps = fromMaybe True $ listToMaybe $ [t | Retry t <- ps]

-- | Top level parser, parses App.
app :: Parser App.App
app = do
    (appName, appProps) <- gaspElementNameAndClosureContent reservedNameApp appProperties

    return App.App
        { App.appName      = appName
        , App.appKey       = getAppKey appProps
        , App.appToken     = getAppToken appProps
        , App.appAddr      = getAppAddr appProps
        , App.appStartAddr = getAppStartAddr appProps
        , App.appCtrl      = getAppCtrl appProps
        , App.appRetry     = getAppRetry appProps
        }
