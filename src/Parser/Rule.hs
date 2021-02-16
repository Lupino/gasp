module Parser.Rule
  ( rule
  ) where

import           Gasp.Rule
import           Lexer
import           Parser.Common
import           Text.Parsec        (anyChar, manyTill, option, try, (<|>))
import           Text.Parsec.String (Parser)

laterP :: Parser String
laterP = do
  _ <- symbol "later"
  identifier <|> (show <$> integer)

elseActionP :: Parser (String, String)
elseActionP = do
  _ <- symbol "else"
  later <- option "" laterP
  v <- identifier
  return (later, v)

-- | rule condition do [later later_ms ]action else [later later_ms ]elseAction
rule :: Parser Rule
rule = do
    reserved reservedNameRule
    condition <- strip <$> manyTill anyChar (try symbolDo)
    later <- option "" laterP
    action <- identifier
    (elseLater, elseAction) <- option ("", "") elseActionP

    return Rule
      { ruleCondition = condition
      , ruleAction = action
      , ruleLater = later
      , ruleElseAction = elseAction
      , ruleElseLater = elseLater
      , ruleIndex = 0
      }

  where symbolDo = symbol "do"
