module Lexer
  ( reservedNameApp
  , reservedNameCommand
  , reservedNameTelemetry
  , reservedNameFunction
  , reservedNameLoop
  , reservedNameSetup
  , reservedNameInit
  , reservedNameAttr
  , reservedNameMetric
  , reservedNameEvery
  , reservedNameGpio
  , reservedNameRule
  , braces
  , brackets
  , symbol
  , bool
  , reserved
  , identifier
  , integer
  , float
  , stringLiteral
  , whiteSpace
  , commaSep
  , commaSep1
  , colon

  , strip
  , block
  , json
  ) where


import           Control.Monad        (unless, void)
import           Data.Aeson           (FromJSON)
import           Data.Functor         (($>))
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Yaml            (decodeEither')
import           Text.Parsec          (alphaNum, anyChar, char, letter, many,
                                       manyTill, oneOf, try, (<|>))
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Token

-- * Wasp element types

reservedNameApp :: String
reservedNameApp = "app"

reservedNameCommand :: String
reservedNameCommand = "command"

reservedNameTelemetry :: String
reservedNameTelemetry = "telemetry"

reservedNameFunction :: String
reservedNameFunction = "func"

reservedNameLoop :: String
reservedNameLoop = "loop"

reservedNameSetup :: String
reservedNameSetup = "setup"

reservedNameInit :: String
reservedNameInit = "init"

reservedNameAttr :: String
reservedNameAttr = "attr"

reservedNameMetric :: String
reservedNameMetric = "metric"

reservedNameEvery :: String
reservedNameEvery = "every"

reservedNameGpio :: String
reservedNameGpio = "gpio"

reservedNameRule :: String
reservedNameRule = "rule"

-- * Data types.

reservedNameBooleanTrue :: String
reservedNameBooleanTrue = "true"

reservedNameBooleanFalse :: String
reservedNameBooleanFalse = "false"

reservedNames :: [String]
reservedNames =
    -- * Wasp element types
    [ reservedNameApp
    , reservedNameCommand
    , reservedNameTelemetry
    , reservedNameFunction
    , reservedNameLoop
    , reservedNameSetup
    , reservedNameInit
    , reservedNameAttr
    , reservedNameMetric
    , reservedNameEvery
    , reservedNameGpio
    , reservedNameRule
    -- * Data types
    , reservedNameBooleanTrue
    , reservedNameBooleanFalse
    ]

gaspLanguageDef :: Token.LanguageDef ()
gaspLanguageDef = emptyDef
    { Token.commentLine = "//"
    , Token.reservedNames = reservedNames
    , Token.caseSensitive = True
    -- Identifier
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '_'
    }

gaspLexer :: Token.TokenParser ()
gaspLexer = Token.makeTokenParser gaspLanguageDef

reserved :: String -> Parser ()
reserved = Token.reserved gaspLexer

identifier :: Parser String
identifier = Token.identifier gaspLexer

symbol :: String -> Parser String
symbol = Token.symbol gaspLexer

colon :: Parser String
colon = Token.colon gaspLexer

braces :: Parser a -> Parser a
braces = Token.braces gaspLexer

brackets :: Parser a -> Parser a
brackets = Token.brackets gaspLexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep gaspLexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 gaspLexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace gaspLexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral gaspLexer

integer :: Parser Integer
integer = Token.integer gaspLexer

naturalOrFloat :: Parser(Either Integer Double)
naturalOrFloat = Token.naturalOrFloat gaspLexer

_float :: Parser Double
_float = do
  v <- naturalOrFloat
  case v of
    Left vv  -> return $ fromIntegral vv
    Right vv -> return vv

float :: Parser Double
float = do
  sign <- many (oneOf ['-', '+'])
  case sign of
    "-" -> (0-) <$> _float
    ""  -> _float
    "+" -> _float
    _   -> fail $ "unexpected \"" ++ sign ++ "\""

-- * Parsing boolean values

bool :: Parser Bool
bool = boolTrue <|> boolFalse

boolTrue :: Parser Bool
boolTrue = reserved reservedNameBooleanTrue $> True

boolFalse :: Parser Bool
boolFalse = reserved reservedNameBooleanFalse $> False

block :: String -> String -> Parser String
block start end = do
  unless (null start) $ void $ symbol start
  strip <$> manyTill anyChar (try (symbol end))

rmHeadSpace :: String -> String
rmHeadSpace = unlines . map strip . lines


json :: FromJSON a => Parser a
json = do
  v <- rmHeadSpace <$> block "{" "}"

  case decodeEither' (encodeUtf8 $ T.pack v) of
    Left _   -> fail $ '{' : v ++ "}"
    Right vv -> return vv

-- | Removes leading and trailing spaces from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack
