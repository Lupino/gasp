module Parser.AGpio
    ( agpio
    ) where

import           Gasp.AGpio
import           Gasp.Metric        (MetricName (..))
import           Lexer
import           Parser.Gpio        (pin)
import           Text.Parsec        (option)
import           Text.Parsec.String (Parser)

bindLink :: Parser AGpioBind
bindLink = do
  _ <- symbol "link"
  n <- MetricName <$> identifier
  return $ LinkMetric n

bindParser :: Parser AGpioBind
bindParser = do
  _ <- symbol "->"
  bindLink

--
-- agpio agpioName pinName [-> link attrName]

-- | Top level parser, parses AGpio.
agpio :: Parser AGpio
agpio = do
  reserved reservedNameAGpio
  name <- identifier
  p <- pin
  b <- option NoBind bindParser

  return AGpio
        { agpioName    = name
        , agpioPin     = p
        , agpioBind    = b
        }
