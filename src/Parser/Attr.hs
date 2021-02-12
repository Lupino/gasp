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
    = Type   !String
    | Max    !Double
    | Min    !Double
    | Def    !Double
    | GenSet !Bool
    | Keep   !Bool
    | Scale  !Double
    | Prec   !Integer
    deriving (Show, Eq)

-- | Parses gasp property along with the key, "key: value".
cusL :: Parser AttrProperty
cusL = do
  key <- identifier
  _ <- colon
  case key of
    "type"    -> Type <$> stringLiteral
    "min"     -> Min <$> float
    "max"     -> Max <$> float
    "default" -> Def <$> float
    "scale"   -> Scale <$> float
    "gen_set" -> GenSet <$> bool
    "keep"    -> Keep <$> bool
    "prec"    -> Prec <$> integer
    _         -> fail $ "no such " ++ key

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
attrProperties :: Parser [AttrProperty]
attrProperties = commaSep1 cusL

getAttrType :: String -> [AttrProperty] -> String
getAttrType def ps = fromMaybe def . listToMaybe $ [t | Type t <- ps]

getAttrMax :: Double -> [AttrProperty] -> Double
getAttrMax def ps = fromMaybe def . listToMaybe $ [t | Max t <- ps]

getAttrMin :: Double -> [AttrProperty] -> Double
getAttrMin def ps = fromMaybe def . listToMaybe $ [t | Min t <- ps]

getAttrDef :: Double -> [AttrProperty] -> Double
getAttrDef def ps = fromMaybe def . listToMaybe $ [t | Def t <- ps]

getAttrScale :: Double -> [AttrProperty] -> Double
getAttrScale def ps = fromMaybe def . listToMaybe $ [t | Scale t <- ps]

getAttrGenSet :: [AttrProperty] -> Bool
getAttrGenSet ps = fromMaybe True . listToMaybe $ [t | GenSet t <- ps]

getAttrKeep :: [AttrProperty] -> Bool
getAttrKeep ps = fromMaybe True . listToMaybe $ [t | Keep t <- ps]

getAttrPrec :: Integer -> [AttrProperty] -> Integer
getAttrPrec def ps = fromMaybe def . listToMaybe $ [t | Prec t <- ps]

-- | Top level parser, parses Attr.
attr :: Parser Attr.Attr
attr = do
    (attrName, attrProps) <- gaspElementNameAndClosureContent reservedNameAttr attrProperties

    return Attr.Attr
        { Attr.attrName   = attrName
        , Attr.attrAddr   = 0
        , Attr.attrType   = getAttrType   "int" attrProps
        , Attr.attrMax    = getAttrMax    100 attrProps
        , Attr.attrMin    = getAttrMin    0 attrProps
        , Attr.attrDef    = getAttrDef    0 attrProps
        , Attr.attrGenSet = getAttrGenSet attrProps
        , Attr.attrKeep   = getAttrKeep   attrProps
        , Attr.attrScale  = getAttrScale  1 attrProps
        , Attr.attrPrec   = fromIntegral $ getAttrPrec 2 attrProps
        }
