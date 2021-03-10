module Gasp.Common
  ( calcWidth
  , isFloat
  , dataLength
  , isUnsigned
  , maxValue
  , minValue
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

maxValue :: String -> Double
maxValue "boolean"       = 1
maxValue "bool"          = 1
maxValue "char"          = 127
maxValue "unsigned char" = 255
maxValue "byte"          = 255
maxValue "int"           = 32767
maxValue "unsigned int"  = 65535
maxValue "word"          = 65535
maxValue "long"          = 2147483647
maxValue "unsigned long" = 2^32-1
maxValue "short"         = 32767
maxValue "float"         = 3.4028235E+38
maxValue "double"        = 3.4028235E+38
maxValue "uint8_t"       = 255
maxValue "uint16_t"      = 65535
maxValue "uint32_t"      = 2^32-1
maxValue _               = 100

minValue :: String -> Double
minValue "boolean"       = 0
minValue "bool"          = 0
minValue "char"          = -128
minValue "unsigned char" = 0
minValue "byte"          = 0
minValue "int"           = -32768
minValue "unsigned int"  = 0
minValue "word"          = 0
minValue "long"          = -2147483648
minValue "unsigned long" = 0
minValue "short"         = -32768
minValue "float"         = -3.4028235E+38
minValue "double"        = -3.4028235E+38
minValue "uint8_t"       = 0
minValue "uint16_t"      = 0
minValue "uint32_t"      = 0
minValue _               = 0
