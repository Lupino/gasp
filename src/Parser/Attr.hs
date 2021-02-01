module Parser.Attr
    ( attr
    ) where

import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Gasp.Attr          as Attr
import           Lexer
import           Parser.Common

-- | A type that describes supported app properties.
data AttrProperty
    = Var   !String
    | Type  !String
    | Max   !String
    | Min   !String
    | Def   !String
    | Scale !String
    deriving (Show, Eq)

-- | Parses gasp property along with the key, "key: value".
cusL :: Parser (String, String)
cusL = do
  key <- identifier
  _ <- colon
  v <- case key of
         "var"  -> identifier
         "type" -> stringLiteral
         _      -> floatString
  return (key, v)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
attrProperties :: Parser [AttrProperty]
attrProperties = map toAttrProperty <$> commaSep1 cusL

toAttrProperty :: (String, String) -> AttrProperty
toAttrProperty ("var", v)     = Var v
toAttrProperty ("type", v)    = Type v
toAttrProperty ("max", v)     = Max v
toAttrProperty ("min", v)     = Min v
toAttrProperty ("default", v) = Def v
toAttrProperty ("scale", v)   = Scale v
toAttrProperty  (k, v)        = error $ "not such " ++ k ++ ": " ++ v

getAttrVar :: String -> [AttrProperty] -> String
getAttrVar def ps = fromMaybe def . listToMaybe $ [t | Var t <- ps]

getAttrType :: String -> [AttrProperty] -> String
getAttrType def ps = fromMaybe def . listToMaybe $ [t | Type t <- ps]

getAttrMax :: String -> [AttrProperty] -> String
getAttrMax def ps = fromMaybe def . listToMaybe $ [t | Max t <- ps]

getAttrMin :: String -> [AttrProperty] -> String
getAttrMin def ps = fromMaybe def . listToMaybe $ [t | Min t <- ps]

getAttrDef :: String -> [AttrProperty] -> String
getAttrDef def ps = fromMaybe def . listToMaybe $ [t | Def t <- ps]

getAttrScale :: String -> [AttrProperty] -> String
getAttrScale def ps = fromMaybe def . listToMaybe $ [t | Scale t <- ps]

-- | Top level parser, parses Attr.
attr :: Parser Attr.Attr
attr = do
    (attrName, attrProps) <- gaspElementNameAndClosureContent reservedNameAttr attrProperties

    let scale = read (getAttrScale "1" attrProps) :: Int
        def   = read (getAttrDef "0" attrProps) :: Int

    return Attr.Attr
        { Attr.attrName  = attrName
        , Attr.attrAddr  = "0"
        , Attr.attrVar   = getAttrVar   attrName attrProps
        , Attr.attrType  = getAttrType  "int" attrProps
        , Attr.attrMax   = getAttrMax   "10000" attrProps
        , Attr.attrMin   = getAttrMin   "0" attrProps
        , Attr.attrDef   = show (def * scale)
        , Attr.attrScale = getAttrScale "1" attrProps
        }
