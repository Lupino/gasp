module Gasp.Attr
    ( Attr(..)
    , getTotalAttrLength
    , setAttrLength
    , getAttrValueLength
    , getAttrRspLength
    , calcWidth
    , isFloatAttr
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
