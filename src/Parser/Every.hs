module Parser.Every
  ( every
  ) where

import           Gasp.Every
import           Lexer
import           Parser.Common
import           Text.Parsec.String (Parser)

every :: Parser Every
every = do
    (name, delay) <- gaspElementNameAndClosure reservedNameEvery integer

    return Every
      { everyFunc    = name
      , everyDelayMs = delay
      }
