module Gasp.Function
  ( Function (..)
  , hasJson
  , hasRetval
  , hasToken
  , FuncName (..)
  , FuncFlag (..)
  , genFuncFlag
  , Arg
  , mkArg
  , assignLast
  , funcFlagToArgv
  ) where


import           Data.Aeson  (ToJSON (..), object, (.=))
import           Data.Text   (Text, stripStart)
import qualified Data.Text   as T (pack)
import           Gasp.Common (GetCode (..), GetName (..), hasToken)

data FuncFlag = FuncFlag
  { flagJson   :: !Bool
  , flagFunc   :: !FuncName
  , flagRetval :: !Bool
  } deriving (Show)

instance Eq FuncFlag where
  x == y = flagFunc x == flagFunc y

genFuncFlag :: FuncName -> FuncFlag
genFuncFlag func = FuncFlag
  { flagJson   = False
  , flagFunc   = func
  , flagRetval = False
  }

instance ToJSON FuncFlag where
  toJSON flag = object
    [ "json"   .= flagJson flag
    , "retval" .= flagRetval flag
    ]

newtype FuncName = FuncName String
  deriving (Show, Eq)

instance ToJSON FuncName where
  toJSON (FuncName n) = toJSON n

data Arg = Arg
  { argName   :: String
  , argType   :: String
  , argIsLast :: Bool
  }
  deriving (Show, Eq)

instance ToJSON Arg where
  toJSON (Arg n t l) = object
    [ "name" .= n
    , "type" .= t
    , "last" .= l
    ]

mkArg :: String -> String -> Arg
mkArg n t = Arg n t False

assignLast :: [Arg] -> [Arg]
assignLast []     = []
assignLast [arg]  = [arg {argIsLast = True}]
assignLast (x:xs) = x: assignLast xs

argJson :: Arg
argJson = mkArg "json" "const char *"

argTokens :: Arg
argTokens = mkArg "tokens" "jsmntok_t *"

argNumToken :: Arg
argNumToken = mkArg "num_tokens" "int"

argRetVal :: Arg
argRetVal = mkArg "retval" "char *"

funcFlagToArgv :: Arg -> FuncFlag -> [Arg]
funcFlagToArgv tokens flag
  | retval && json = [argJson, tokens, argNumToken, argRetVal]
  | retval = [argRetVal]
  | json   = [argJson, tokens, argNumToken]
  | otherwise  = []
  where retval = flagRetval flag
        json   = flagJson flag

data Function = Function
  { funcName :: !FuncName
  , funcCode :: !Text
  , funcFlag :: !FuncFlag
  , funcArgv :: ![Arg]
  , funcType :: !String
  } deriving (Show)

instance Eq Function where
  c0 == c1 = funcName c0 == funcName c1

instance ToJSON Function where
  toJSON func = object
    [ "name"     .= funcName func
    , "code"     .= stripStart (funcCode func)
    , "argv"     .= assignLast argv
    , "type"     .= funcType func
    ]
    where argv = if null (funcArgv func) then funcFlagToArgv argTokens (funcFlag func)
                                         else funcArgv func

instance GetName Function where
  getName func = T.pack fn
    where FuncName fn = funcName func

instance GetCode Function where
  getCode = funcCode

hasRetval :: Function -> Bool
hasRetval = hasToken "retval" . (" " <>) . funcCode

hasJson :: Function -> Bool
hasJson = hasToken "tokens" . (" " <>) . funcCode
