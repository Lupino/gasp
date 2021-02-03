module Parser.Telemetry
  ( telemetry
  ) where

import           Text.Parsec.String (Parser)

import           Gasp.Flag          (defFlag)
import qualified Gasp.Telemetry     as Telem
import qualified Lexer              as L

telemetry :: Parser Telem.Telemetry
telemetry = do
    L.reserved L.reservedNameTelemetry
    func <- L.identifier

    return Telem.Telemetry
      { Telem.telemFunc = func
      , Telem.telemFlag = defFlag
      }
