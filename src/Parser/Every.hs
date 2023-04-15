module Parser.Every
  ( every
  ) where

import           Gasp.Every
import           Lexer
import           Parser.Common
import           Text.Parsec        (option, (<|>))
import           Text.Parsec.String (Parser)

every :: Parser Every
every = do
    (coreName, name, delay) <- gaspElementNameAndClosureWithCore reservedNameEvery $ (show <$> integer) <|> identifier

    on <- option "" $ block "on" "\n"

    return Every
      { everyCore    = coreName
      , everyFunc    = name
      , everyDelayMs = delay
      , everyOn      = on
      , everyIdx     = 0
      }
