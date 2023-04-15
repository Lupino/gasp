module Parser.Linkage
  ( linkage
  ) where

import           Gasp.Function      (FuncName (..))
import           Gasp.Linkage
import           Lexer
import           Text.Parsec.String (Parser)

linkage :: Parser Linkage
linkage = do
  reserved reservedNameLinkage
  core <- coreId
  name <- identifier
  fnG <- FuncName <$> identifier
  fn0 <- FuncName <$> identifier
  flip (Linkage core name fnG fn0) 0 . FuncName <$> identifier
