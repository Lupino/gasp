module Gasp.Attr
    ( Attr(..)
    , getTotalAttrLength
    , setAttrLength
    , getAttrValueLength
    , getAttrRspLength
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
        , "is_float"   .= isFloat (attrType attr)
        , "gen_set"    .= attrGenSet attr
        , "default"    .= (attrDef   attr * attrScale attr)
        , "width"      .= calcWitdh (attrMax attr)
        , "prec"       .= attrPrec   attr
        , "keep"       .= attrKeep   attr
        ]

calcWitdh :: Double -> Int
calcWitdh v = length $ show (floor v :: Int)

isFloat :: String -> Bool
isFloat ""                      = False
isFloat ('f':'l':'o':'a':'t':_) = True
isFloat (_:xs)                  = isFloat xs

-- {"name": vv.vv}
-- {"name": vv}
getAttrRspLength :: Attr -> Int
getAttrRspLength attr
  | isFloat (attrType attr) = 6 + length (attrName attr) + calcWitdh (attrMax attr) + 1 + attrPrec attr
  | otherwise = 6 + length (attrName attr) + calcWitdh (attrMax attr)

-- {"method": "set_name", "data": vv.vv}
-- {"method": "set_name", "data": vv}
setAttrLength :: Attr -> Int
setAttrLength attr
  | isFloat (attrType attr) = 28 + length (attrName attr) + calcWitdh (attrMax attr) + 1 + attrPrec attr
  | otherwise = 28 + length (attrName attr) + calcWitdh (attrMax attr)

getTotalAttrLength :: Int -> [Attr] -> Int
getTotalAttrLength v []     = v
getTotalAttrLength v (x:xs) = getTotalAttrLength (v + getAttrRspLength x) xs

getAttrValueLength :: Attr -> Int
getAttrValueLength attr
  | isFloat (attrType attr) = calcWitdh (attrMax attr) + 1 + attrPrec attr
  | otherwise = calcWitdh (attrMax attr)
