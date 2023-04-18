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
  identifier <|> (show <$> integer) <|> block "(" ")"

elseActionP :: Parser (String, String)
elseActionP = try $ do
  _ <- symbol "else"
  later <- option "" laterP
  v <- identifier
  return (later, v)

onP :: Parser String
onP = block "on" "\n"

onF :: Parser String
onF = try $ do
  _ <- symbol "onF"
  identifier

-- | rule condition do [later later_ms ]action else [later later_ms ]elseAction
rule :: Parser Rule
rule = do
    reserved reservedNameRule
    core <- coreId
    condition <- block "" "do"
    later <- option "" laterP
    action <- identifier
    (elseLater, elseAction) <- option ("", "") elseActionP
    forceCond <- option "" onF
    onCondition <- option "" onP

    return Rule
      { ruleCore = core
      , ruleCondition = condition
      , ruleOnCondition = onCondition
      , ruleForceCond = forceCond
      , ruleAction = action
      , ruleLater = later
      , ruleElseAction = elseAction
      , ruleElseLater = elseLater
      , ruleIndex = 0
      }
