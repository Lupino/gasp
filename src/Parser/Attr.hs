module Parser.Attr
    ( attr
    ) where

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

-- | Top level parser, parses Attr.
attr :: Parser Attr.Attr
attr = do
    (attrName, props) <- gaspElementNameAndClosureContent reservedNameAttr attrProperties

    let tp = getFromList (DataType "int") [t | Type t <- props]
        tpMax = maxValue tp
        tpMin = minValue tp
        scale = getFromList 1 [t | Scale t <- props]
        unScaledMax = tpMax / scale
        unScaledMin = tpMin / scale

    return Attr.Attr
        { Attr.attrName   = Attr.AttrName attrName
        , Attr.attrAddr   = 0
        , Attr.attrType   = tp
        , Attr.attrMax    = min unScaledMax (getFromList unScaledMax [t | Max t <- props])
        , Attr.attrMin    = max unScaledMin (getFromList unScaledMin [t | Min t <- props])
        , Attr.attrDef    = getFromList 0 [t | Def t <- props]
        , Attr.attrGenSet = getFromList True [t | GenSet t <- props]
        , Attr.attrKeep   = getFromList True [t | Keep t <- props]
        , Attr.attrScale  = scale
        , Attr.attrPrec   = fromIntegral $ getFromList 2 [t | Prec t <- props]
        }
