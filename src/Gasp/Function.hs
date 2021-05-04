module Gasp.Function
    ( Function (..)
    , hasJson
    , hasRetval
    , hasToken
    , FuncName (..)
    , getRequiredFunction
    ) where


import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.List  (partition)
import           Data.Text  (Text)
import qualified Data.Text  as T (breakOnEnd, dropEnd, intercalate, length,
                                  null, pack, take, takeEnd, takeWhileEnd)
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
    } deriving (Show)

instance Eq Function where
  c0 == c1 = funcName c0 == funcName c1

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
        validC = [" ", ",", "=", "(", ")", "[", "]", ";", "\n", "!", ".", "+", "-", "*", "/", "&", ">", "<"]
        isToken = endC `elem` validC && startC `elem` validC
        prevLine = T.takeWhileEnd (/='\n') prev
        (comment, _) = T.breakOnEnd "//" prevLine
        isNotComment = T.null comment

hasRetval :: Function -> Bool
hasRetval = hasToken "retval" . funcCode

hasJson :: Function -> Bool
hasJson = hasToken "tokens" . funcCode


isRequired :: Text -> Function -> Bool
isRequired txt func = hasToken (T.pack fn) txt
  where FuncName fn = funcName func

splitRequired :: Text -> [Function] -> ([Function], [Function])
splitRequired = partition . isRequired

getRequiredFunction :: Text -> [Function] -> [Function]
getRequiredFunction txt funcs
  | null required = []
  | otherwise     = required ++ getRequiredFunction nextText unrequired
  where (required, unrequired) = splitRequired txt funcs
        nextText = T.intercalate "\n" $ map funcCode required
