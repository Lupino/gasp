module Parser.Every
  ( every
  ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Gasp.Every
import           Lexer
import           Parser.Common

every :: Parser Every
every = do
    (name, delay) <- gaspElementNameAndClosure reservedNameEvery integer

    return Every
      { everyFunc    = name
      , everyDelayMs = delay
      }
