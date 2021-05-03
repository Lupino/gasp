module Gasp.Function
    ( Function (..)
    , hasJson
    , hasRetval
    , hasToken
    , FuncName (..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)
import qualified Data.Text  as T (breakOnEnd, dropEnd, length, null, take,
                                  takeEnd, takeWhileEnd)
import           Gasp.Flag  (Flag)

newtype FuncName = FuncName String
  deriving (Show, Eq)

instance ToJSON FuncName where
  toJSON (FuncName n) = toJSON n

data Function = Function
    { funcName :: !FuncName
    , funcCode :: !Text
    , funcFlag :: !Flag
    , funcArgv :: !String
    , funcType :: !String
    } deriving (Show, Eq)

instance ToJSON Function where
    toJSON func = object
        [ "name"     .= funcName func
        , "code"     .= funcCode func
        , "flag"     .= funcFlag func
        , "argv"     .= funcArgv func
        , "has_argv" .= not (null $ funcArgv func)
        , "type"     .= funcType func
        ]

hasToken :: Text -> Text -> Bool
hasToken tok txt
  | isNotComment && isToken = True
  | tokLen > prevLen = False
  | otherwise = hasToken tok $ T.dropEnd tokLen prev
  where (prev, next) = T.breakOnEnd tok txt
        tokLen = T.length tok
        prevLen = T.length prev
        endC = T.take 1 next
        startC = T.take 1 $ T.takeEnd (tokLen + 1) prev
        validEndC = [" ", ",", ")", "="]
        validStartC = [" ", ",", "("]
        isToken = endC `elem` validEndC && startC `elem` validStartC
        prevLine = T.takeWhileEnd (/='\n') prev
        (comment, _) = T.breakOnEnd "//" prevLine
        isNotComment = T.null comment

hasRetval :: Function -> Bool
hasRetval = hasToken "retval" . funcCode

hasJson :: Function -> Bool
hasJson = hasToken "tokens" . funcCode
