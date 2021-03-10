module Gasp.Common
  ( calcWidth
  , isFloat
  , dataLength
  , isUnsigned
  ) where

calcWidth :: Double -> Double -> Int
calcWidth v0 v1 =
  max (length $ show (floor v0 :: Int)) (length $ show (floor v1 :: Int))

isFloat :: String -> Bool
isFloat ""                      = False
isFloat ('f':'l':'o':'a':'t':_) = True
isFloat (_:xs)                  = isFloat xs

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
