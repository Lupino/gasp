module Parser.Rule
  ( rule
  ) where

import qualified Data.Text          as T (pack)
import           Gasp.Rule
import           Lexer
import           Parser.Common
import           Text.Parsec        (anyChar, manyTill, option, try)
import           Text.Parsec.String (Parser)

elseActionP :: Parser String
elseActionP = do
  _ <- symbol "else"
  identifier

-- | rule condition do action else elseAction
rule :: Parser Rule
rule = do
    reserved reservedNameRule
    condition <- strip <$> manyTill anyChar (try symbolDo)
    action <- identifier
    elseAction <- option "" elseActionP

    return Rule
      { ruleCondition = T.pack condition
      , ruleAction = action
      , ruleElseAction = elseAction
      }

  where symbolDo = symbol "do"
