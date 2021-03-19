module Parser.Attr
    ( attr
    ) where

import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Gasp.Attr          as Attr
import           Gasp.Common        (DataType (..), maxValue, minValue)
import           Lexer
import           Parser.Common
import           Text.Parsec.String (Parser)

-- | A type that describes supported app properties.
data AttrProperty
    = Type   !DataType
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
    "type"    -> Type <$> dataType
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

getAttrType :: DataType -> [AttrProperty] -> DataType
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

    let tp = getAttrType (DataType "int") attrProps
        tpMax = maxValue tp
        tpMin = minValue tp
        scale = getAttrScale 1 attrProps
        unScaledMax = tpMax / scale
        unScaledMin = tpMin / scale

    return Attr.Attr
        { Attr.attrName   = Attr.AttrName attrName
        , Attr.attrAddr   = 0
        , Attr.attrType   = tp
        , Attr.attrMax    = min unScaledMax (getAttrMax unScaledMax attrProps)
        , Attr.attrMin    = max unScaledMin (getAttrMin unScaledMin attrProps)
        , Attr.attrDef    = getAttrDef 0 attrProps
        , Attr.attrGenSet = getAttrGenSet attrProps
        , Attr.attrKeep   = getAttrKeep attrProps
        , Attr.attrScale  = scale
        , Attr.attrPrec   = fromIntegral $ getAttrPrec 2 attrProps
        }
