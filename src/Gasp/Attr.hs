module Gasp.Attr
    ( Attr(..)
    , getTotalAttrLength
    , setAttrLength
    , getAttrValueLength
    , getAttrRspLength
    , calcWidth
    , isFloatAttr
    , getAttrDataLength
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))

data Attr = Attr
    { attrName   :: !String -- Identifier
    , attrAddr   :: !Int
    , attrMax    :: !Double
    , attrMin    :: !Double
    , attrType   :: !String
    , attrDef    :: !Double
    , attrGenSet :: !Bool
    , attrScale  :: !Double
    , attrPrec   :: !Int
    , attrKeep   :: !Bool
    } deriving (Show, Eq)

instance ToJSON Attr where
    toJSON attr = object
        [ "name"       .= attrName   attr
        , "addr"       .= attrAddr   attr
        , "max"        .= attrMax    attr
        , "min"        .= attrMin    attr
        , "scaled_max" .= (attrMax   attr * attrScale attr)
        , "scaled_min" .= (attrMin   attr * attrScale attr)
        , "scale"      .= attrScale  attr
        , "type"       .= attrType   attr
        , "is_float"   .= isFloatAttr attr
        , "uncheckmin" .= (isUnsigned (attrType attr) && attrMin attr == 0)
        , "gen_set"    .= attrGenSet attr
        , "default"    .= (attrDef   attr * attrScale attr)
        , "width"      .= calcAttrWidth attr
        , "prec"       .= attrPrec   attr
        , "keep"       .= attrKeep   attr
        ]

calcWidth :: Double -> Double -> Int
calcWidth v0 v1 =
  max (length $ show (floor v0 :: Int)) (length $ show (floor v1 :: Int))

calcAttrWidth :: Attr -> Int
calcAttrWidth attr = calcWidth (attrMax attr) (attrMin attr)

isFloat :: String -> Bool
isFloat ""                      = False
isFloat ('f':'l':'o':'a':'t':_) = True
isFloat (_:xs)                  = isFloat xs

isFloatAttr :: Attr -> Bool
isFloatAttr = isFloat . attrType

-- {"name": vv.vv}
-- {"name": vv}
getAttrRspLength :: Attr -> Int
getAttrRspLength attr = 6 + length (attrName attr) + getAttrValueLength attr

-- {"method": "set_name", "data": vv.vv}
-- {"method": "set_name", "data": vv}
setAttrLength :: Attr -> Int
setAttrLength attr = 28 + length (attrName attr) + getAttrValueLength attr

getTotalAttrLength :: Int -> [Attr] -> Int
getTotalAttrLength v []     = v
getTotalAttrLength v (x:xs) = getTotalAttrLength (v + getAttrRspLength x) xs

getAttrValueLength :: Attr -> Int
getAttrValueLength attr
  | isFloatAttr attr = calcAttrWidth attr + 1 + attrPrec attr
  | otherwise = calcAttrWidth attr


dataLength :: String -> Int
dataLength "boolean"       = 1
dataLength "bool"          = 1
dataLength "char"          = 1
dataLength "unsigned char" = 1
dataLength "byte"          = 1
dataLength "int"           = 2
dataLength "unsigned int"  = 2
dataLength "word"          = 2
dataLength "long"          = 4
dataLength "unsigned long" = 4
dataLength "short"         = 2
dataLength "float"         = 4
dataLength "double"        = 4
dataLength "uint8_t"       = 1
dataLength "uint16_t"      = 2
dataLength "uint32_t"      = 4
dataLength _               = 4

isUnsigned :: String -> Bool
isUnsigned "boolean"       = True
isUnsigned "bool"          = True
isUnsigned "char"          = False
isUnsigned "unsigned char" = True
isUnsigned "byte"          = False
isUnsigned "int"           = False
isUnsigned "unsigned int"  = True
isUnsigned "word"          = True
isUnsigned "long"          = False
isUnsigned "unsigned long" = True
isUnsigned "short"         = False
isUnsigned "float"         = False
isUnsigned "double"        = False
isUnsigned "uint8_t"       = True
isUnsigned "uint16_t"      = True
isUnsigned "uint32_t"      = True
isUnsigned _               = False

getAttrDataLength :: Attr -> Int
getAttrDataLength = dataLength . attrType
