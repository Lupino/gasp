module Parser.Rule
  ( rule
  ) where


import           Gasp.Rule
import           Lexer
import           Text.Parsec        (option, try, (<|>))
import           Text.Parsec.String (Parser)

laterP :: Parser String
laterP = try $ do
  _ <- symbol "later"
  identifier <|> (show <$> integer)

elseActionP :: Parser (String, String)
elseActionP = try $ do
  _ <- symbol "else"
  later <- option "" laterP
  v <- identifier
  return (later, v)


onP :: Parser String
onP = block "on" "\n"

-- | rule condition do [later later_ms ]action else [later later_ms ]elseAction
rule :: Parser Rule
rule = do
    reserved reservedNameRule
    condition <- block "" "do"
    later <- option "" laterP
    action <- identifier
    (elseLater, elseAction) <- option ("", "") elseActionP
    onCondition <- option "" onP

    return Rule
      { ruleCondition = condition
      , ruleOnCondition = onCondition
      , ruleAction = action
      , ruleLater = later
      , ruleElseAction = elseAction
      , ruleElseLater = elseLater
      , ruleIndex = 0
      }
