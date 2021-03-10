module Gasp.Attr
    ( Attr(..)
    , getTotalAttrLength
    , setAttrLength
    , getAttrValueLength
    , getAttrRspLength
    , isFloatAttr
    , getAttrDataLength
    ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Gasp.Common

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
        , "uncheckmin" .= (isUnsigned (attrType attr) && attrMin attr <= 0)
        , "onebyte"    .= (getAttrDataLength attr == 1)
        , "gen_set"    .= attrGenSet attr
        , "default"    .= (attrDef   attr * attrScale attr)
        , "width"      .= calcAttrWidth attr
        , "prec"       .= attrPrec   attr
        , "keep"       .= attrKeep   attr
        ]

calcAttrWidth :: Attr -> Int
calcAttrWidth attr = calcWidth (attrMax attr) (attrMin attr)

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

getAttrDataLength :: Attr -> Int
getAttrDataLength = dataLength . attrType
