module Gasp.Function
    ( Function (..)
    , hasJson
    , hasRetval
    , hasToken
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)
import qualified Data.Text  as T (breakOnEnd, dropEnd, length, lines, null,
                                  strip, take, takeEnd, takeWhileEnd, words)
import           Gasp.Flag  (Flag)

data Function = Function
    { funcName :: !String
    , funcCode :: !Text
    , funcFlag :: !Flag
    , funcArgv :: !String
    } deriving (Show, Eq)

instance ToJSON Function where
    toJSON func = object
        [ "name"     .= funcName func
        , "code"     .= funcCode func
        , "flag"     .= funcFlag func
        , "argv"     .= funcArgv func
        , "has_argv" .= not (null $ funcArgv func)
        , "return"   .= lastReturn (funcCode func)
        ]

lastReturn :: Text -> Bool
lastReturn = go . T.lines
  where go :: [Text] -> Bool
        go [] = False
        go xs | T.length lstv > 0 = go1 (init xs) (T.words lstv)
              | otherwise = go $ init xs
          where lstv = T.strip $ last xs

        go1 :: [Text] -> [Text] -> Bool
        go1 _ ("return":_) = True
        go1 xs ("//":_)    = go xs
        go1 _ _            = False


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
