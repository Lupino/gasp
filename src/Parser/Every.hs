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
    (name, delay) <- gaspElementNameAndClosure reservedNameEvery $ (show <$> integer) <|> identifier

    on <- option "" $ block "on" "\n"

    return Every
      { everyFunc    = name
      , everyDelayMs = delay
      , everyOn      = on
      , everyIdx     = 0
      }
