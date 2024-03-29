module Lexer
  ( reservedNameApp
  , reservedNameCommand
  , reservedNameFunction
  , reservedNameLoop
  , reservedNameSetup
  , reservedNameLoop0
  , reservedNameSetup0
  , reservedNameLoop1
  , reservedNameSetup1
  , reservedNameAttr
  , reservedNameMetric
  , reservedNameEvery
  , reservedNameGpio
  , reservedNameAGpio
  , reservedNameRule
  , reservedNameUart
  , reservedNameUartRead
  , reservedNameUartWrite
  , reservedNameRequire
  , reservedNameImport
  , reservedNameTimer
  , reservedNameLinkage
  , reservedNameFlag
  , reservedNameFd
  , reservedNameRaw
  , reservedNameData
  , reservedNameTmpl
  , reservedNameRender
  , reservedNameRender1
  , reservedNameIfEq
  , reservedNameIfNeq
  , reservedNameBin
  , reservedNameStr
  , reservedNameStruct

  , braces
  , brackets
  , symbol
  , bool
  , reserved
  , identifier
  , integer
  , decimal
  , float
  , stringLiteral
  , whiteSpace
  , commaSep
  , commaSep1
  , colon

  , strip
  , block
  , blockC
  , json
  , jsonObject
  , jsonArray

  , coreId
  ) where


import           Control.Monad        (unless, void)
import           Data.Aeson           (FromJSON)
import           Data.Functor         (($>))
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Yaml            (decodeEither')
import           Text.Parsec          (alphaNum, anyChar, char, letter,
                                       lookAhead, many, manyTill, oneOf, option,
                                       try, (<|>))
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Token

-- * Gasp element types

reservedNameApp :: String
reservedNameApp = "app"

reservedNameCommand :: String
reservedNameCommand = "command"

reservedNameFunction :: String
reservedNameFunction = "func"

reservedNameLoop :: String
reservedNameLoop = "loop"

reservedNameSetup :: String
reservedNameSetup = "setup"

reservedNameLoop0 :: String
reservedNameLoop0 = "loop0"

reservedNameSetup0 :: String
reservedNameSetup0 = "setup0"

reservedNameLoop1 :: String
reservedNameLoop1 = "loop1"

reservedNameSetup1 :: String
reservedNameSetup1 = "setup1"

reservedNameAttr :: String
reservedNameAttr = "attr"

reservedNameMetric :: String
reservedNameMetric = "metric"

reservedNameEvery :: String
reservedNameEvery = "every"

reservedNameGpio :: String
reservedNameGpio = "gpio"

reservedNameAGpio :: String
reservedNameAGpio = "agpio"

reservedNameRule :: String
reservedNameRule = "rule"

reservedNameUart :: String
reservedNameUart = "uart"

reservedNameUartRead :: String
reservedNameUartRead = "read"

reservedNameUartWrite :: String
reservedNameUartWrite = "write"

reservedNameRequire :: String
reservedNameRequire = "require"

reservedNameImport :: String
reservedNameImport = "import"

reservedNameTimer :: String
reservedNameTimer = "timer"

reservedNameLinkage :: String
reservedNameLinkage = "linkage"

reservedNameFlag :: String
reservedNameFlag = "flag"

reservedNameFd :: String
reservedNameFd = "fd"

reservedNameRaw :: String
reservedNameRaw = "raw"

reservedNameData :: String
reservedNameData = "data"

reservedNameTmpl :: String
reservedNameTmpl = "tmpl"

reservedNameRender :: String
reservedNameRender = "render"

reservedNameRender1 :: String
reservedNameRender1 = "render1"

reservedNameIfEq :: String
reservedNameIfEq = "ifeq"

reservedNameIfNeq :: String
reservedNameIfNeq = "ifneq"

reservedNameBin :: String
reservedNameBin = "bin"

reservedNameStr :: String
reservedNameStr = "str"

reservedNameStruct :: String
reservedNameStruct = "struct"

-- * Data types.

reservedNameBooleanTrue :: String
reservedNameBooleanTrue = "true"

reservedNameBooleanFalse :: String
reservedNameBooleanFalse = "false"

reservedNames :: [String]
reservedNames =
    -- * Gasp element types
    [ reservedNameApp
    , reservedNameCommand
    , reservedNameFunction
    , reservedNameLoop
    , reservedNameSetup
    , reservedNameLoop0
    , reservedNameSetup0
    , reservedNameLoop1
    , reservedNameSetup1
    , reservedNameAttr
    , reservedNameMetric
    , reservedNameEvery
    , reservedNameGpio
    , reservedNameAGpio
    , reservedNameRule
    , reservedNameUart
    , reservedNameUartRead
    , reservedNameUartWrite
    , reservedNameRequire
    , reservedNameImport
    , reservedNameTimer
    , reservedNameLinkage
    , reservedNameFlag
    , reservedNameFd
    , reservedNameRaw
    , reservedNameData
    , reservedNameTmpl
    , reservedNameRender
    , reservedNameRender1
    , reservedNameIfEq
    , reservedNameIfNeq
    , reservedNameBin
    , reservedNameStr
    , reservedNameStruct
    -- * Data types
    , reservedNameBooleanTrue
    , reservedNameBooleanFalse
    ]

gaspLanguageDef :: Token.LanguageDef ()
gaspLanguageDef = emptyDef
    { Token.commentLine   = "//"
    , Token.commentStart  = "/*"
    , Token.commentEnd    = "*/"
    , Token.reservedNames = reservedNames
    , Token.caseSensitive = True
    -- Identifier
    , Token.identStart    = letter <|> char '_'
    , Token.identLetter   = alphaNum <|> char '_'
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

decimal :: Parser Integer
decimal = Token.decimal gaspLexer

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

blockC :: Char -> Char -> Parser String
blockC start end = do
  whiteSpace
  void $ lookAhead $ char start
  v <- drop 1 . strip <$> block0 0 start end
  whiteSpace
  return v

block0 :: Int -> Char -> Char -> Parser String
block0 n startC endC = block1 n startC endC =<< anyChar

block1 :: Int -> Char -> Char -> Char -> Parser String
block1 n startC endC c
  | c == startC = (c:) <$> block0 (n + 1) startC endC
  | c /= endC = (c:) <$> block0 n startC endC
  | n <= 1 = return []
  | otherwise = (c:) <$> block0 (n - 1) startC endC

isFstSpace :: [String] -> Bool
isFstSpace []           = True
isFstSpace ((' ':_):xs) = isFstSpace xs
isFstSpace _            = False

rmHeadSpace :: String -> String
rmHeadSpace = unlines . go . lines
  where go :: [String] -> [String]
        go [] = []
        go xs
          | isFstSpace xs = go (map (drop 1) xs)
          | otherwise = xs


json :: FromJSON a => Char -> Char -> Parser a
json start end = do
  v <- rmHeadSpace <$> blockC start end
  case decodeEither' (encodeUtf8 $ T.pack v) of
    Left _   -> fail $ [start]  ++ v ++ [end]
    Right vv -> return vv

jsonObject :: FromJSON a => Parser a
jsonObject = json '{' '}'


jsonArray :: FromJSON a => Parser a
jsonArray = json '[' ']'

coreId :: Parser String
coreId = option "0" $ try $ do
  void $ symbol "core"
  cid <- oneOf ['0', '1']
  whiteSpace
  return [cid]

-- | Removes leading and trailing spaces from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack
