module Gasp.Common
  ( calcWidth
  , isFloat
  , dataLength
  , isUnsigned
  , maxValue
  , minValue
  , isLong
  , DataType (..)
  ) where

import           Data.Aeson (ToJSON (..))

newtype DataType = DataType String
  deriving (Show, Eq)

instance ToJSON DataType where
  toJSON (DataType tp) = toJSON tp

calcWidth :: Double -> Double -> Int
calcWidth v0 v1 =
  max (length $ show (floor v0 :: Int)) (length $ show (floor v1 :: Int))

isFloat :: DataType -> Bool
isFloat (DataType "float")  = True
isFloat (DataType "double") = True
isFloat _                   = False

dataLength :: DataType -> Int
dataLength (DataType "boolean")       = 1
dataLength (DataType "bool")          = 1
dataLength (DataType "char")          = 1
dataLength (DataType "unsigned char") = 1
dataLength (DataType "byte")          = 1
dataLength (DataType "int")           = 2
dataLength (DataType "unsigned int")  = 2
dataLength (DataType "word")          = 2
dataLength (DataType "long")          = 4
dataLength (DataType "unsigned long") = 4
dataLength (DataType "short")         = 2
dataLength (DataType "float")         = 4
dataLength (DataType "double")        = 4
dataLength (DataType "uint8_t")       = 1
dataLength (DataType "uint16_t")      = 2
dataLength (DataType "uint32_t")      = 4
dataLength _                          = 4

isUnsigned :: DataType -> Bool
isUnsigned (DataType "boolean")       = True
isUnsigned (DataType "bool")          = True
isUnsigned (DataType "char")          = False
isUnsigned (DataType "unsigned char") = True
isUnsigned (DataType "byte")          = False
isUnsigned (DataType "int")           = False
isUnsigned (DataType "unsigned int")  = True
isUnsigned (DataType "word")          = True
isUnsigned (DataType "long")          = False
isUnsigned (DataType "unsigned long") = True
isUnsigned (DataType "short")         = False
isUnsigned (DataType "float")         = False
isUnsigned (DataType "double")        = False
isUnsigned (DataType "uint8_t")       = True
isUnsigned (DataType "uint16_t")      = True
isUnsigned (DataType "uint32_t")      = True
isUnsigned _                          = False

maxValue :: DataType -> Double
maxValue (DataType "boolean")       = 1
maxValue (DataType "bool")          = 1
maxValue (DataType "char")          = 127
maxValue (DataType "unsigned char") = 255
maxValue (DataType "byte")          = 255
maxValue (DataType "int")           = 32767
maxValue (DataType "unsigned int")  = 65535
maxValue (DataType "word")          = 65535
maxValue (DataType "long")          = 2147483647
maxValue (DataType "unsigned long") = 4294967295
maxValue (DataType "short")         = 32767
maxValue (DataType "float")         = 3.4028235E+38
maxValue (DataType "double")        = 3.4028235E+38
maxValue (DataType "uint8_t")       = 255
maxValue (DataType "uint16_t")      = 65535
maxValue (DataType "uint32_t")      = 4294967295
maxValue _                          = 100

minValue :: DataType -> Double
minValue (DataType "boolean")       = 0
minValue (DataType "bool")          = 0
minValue (DataType "char")          = -128
minValue (DataType "unsigned char") = 0
minValue (DataType "byte")          = 0
minValue (DataType "int")           = -32768
minValue (DataType "unsigned int")  = 0
minValue (DataType "word")          = 0
minValue (DataType "long")          = -2147483648
minValue (DataType "unsigned long") = 0
minValue (DataType "short")         = -32768
minValue (DataType "float")         = -3.4028235E+38
minValue (DataType "double")        = -3.4028235E+38
minValue (DataType "uint8_t")       = 0
minValue (DataType "uint16_t")      = 0
minValue (DataType "uint32_t")      = 0
minValue _                          = 0

isLong :: DataType -> Bool
isLong (DataType "uint32_t")      = True
isLong (DataType "unsigned long") = True
isLong (DataType "long")          = True
isLong _                          = False
