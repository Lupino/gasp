module Gasp.Attr
    ( Attr(..)
    , getTotalAttrLength
    , setAttrLength
    , getAttrValueLength
    , getAttrRspLength
    , isFloatAttr
    , getAttrDataLength

    , AttrName (..)
    ) where

import           Data.Aeson  (ToJSON (..), object, (.=))
import           Gasp.Common

newtype AttrName = AttrName String
  deriving (Show, Eq)

instance ToJSON AttrName where
  toJSON (AttrName n) = toJSON n

data Attr = Attr
    { attrName   :: !AttrName -- Identifier
    , attrAddr   :: !Int
    , attrMax    :: !Double
    , attrMin    :: !Double
    , attrType   :: !DataType
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
        , "max"        .= min unScaledMax (attrMax attr)
        , "min"        .= max unScaledMin (attrMin attr)
        , "scaled_max" .= min tpMax (attrMax attr * attrScale attr)
        , "scaled_min" .= max tpMin (attrMin attr * attrScale attr)
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
        , "unsigned"   .= isUnsigned (attrType attr)
        ]
          where tpMax = maxValue (attrType attr)
                tpMin = minValue (attrType attr)
                unScaledMax = tpMax / attrScale attr
                unScaledMin = tpMin / attrScale attr

calcAttrWidth :: Attr -> Int
calcAttrWidth attr = calcWidth (attrMax attr) (attrMin attr)

isFloatAttr :: Attr -> Bool
isFloatAttr = isFloat . attrType

attrNameLen :: Attr -> Int
attrNameLen = length . t . attrName
  where t :: AttrName -> String
        t (AttrName n) = n

-- {"name": vv.vv}
-- {"name": vv}
getAttrRspLength :: Attr -> Int
getAttrRspLength attr = 6 + attrNameLen attr + getAttrValueLength attr

-- {"method": "set_name", "data": vv.vv}
-- {"method": "set_name", "data": vv}
setAttrLength :: Attr -> Int
setAttrLength attr = 28 + attrNameLen attr + getAttrValueLength attr

getTotalAttrLength :: Int -> [Attr] -> Int
getTotalAttrLength v []     = v
getTotalAttrLength v (x:xs) = getTotalAttrLength (v + getAttrRspLength x) xs

getAttrValueLength :: Attr -> Int
getAttrValueLength attr
  | isFloatAttr attr = calcAttrWidth attr + 1 + attrPrec attr
  | otherwise = calcAttrWidth attr

getAttrDataLength :: Attr -> Int
getAttrDataLength = dataLength . attrType
