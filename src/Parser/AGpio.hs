module Parser.AGpio
    ( agpio
    ) where

import           Gasp.AGpio
import           Lexer
import           Text.Parsec        (option, (<|>))
import           Text.Parsec.String (Parser)

bindParser :: AGpio -> Parser AGpio
bindParser g = do
  _ <- symbol "->"
  _ <- symbol "link"
  n <- identifier

  return g { agpioLink = n }

--
-- agpio agpioName pinName [-> link attrName]

-- | Top level parser, parses AGpio.
agpio :: Parser AGpio
agpio = do
  reserved reservedNameAGpio
  name <- identifier
  pin <- stringLiteral <|> (show <$> integer) <|> identifier

  let g = AGpio
        { agpioName    = name
        , agpioPin     = pin
        , agpioLink    = ""
        }

  option g $ bindParser g
