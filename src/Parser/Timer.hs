module Parser.Timer
  ( timer
  ) where

import           Gasp.Function      (FuncName (..))
import           Gasp.Timer
import           Lexer
import           Text.Parsec.String (Parser)

timer :: Parser Timer
timer = do
  reserved reservedNameTimer
  name <- identifier
  fn0 <- FuncName <$> identifier
  flip (Timer name fn0) 0 . FuncName <$> identifier
