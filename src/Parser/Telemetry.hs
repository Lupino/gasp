module Parser.Telemetry
  ( telemetry
  ) where

import           Text.Parsec.String (Parser)

import           Gasp.Flag          (defFlag)
import qualified Gasp.Telemetry     as Telem
import qualified Lexer              as L
import qualified Parser.Common      as P

telemetry :: Parser Telem.Telemetry
telemetry = do
    L.reserved L.reservedNameTelemetry
    telemProperties <- P.gaspClosure (L.commaSep1 telemProperty)

    return Telem.Telemetry
        { Telem.telemFunc = getTelemFunc telemProperties
        , Telem.telemFlag = defFlag
        }

-- Auxiliary data structure used by parser.
data TelemetryProperty = Func String

getTelemFunc :: [TelemetryProperty] -> String
getTelemFunc props = head [s | Func s <- props]

-- Sub-parsers

telemProperty :: Parser TelemetryProperty
telemProperty = telemPropertyFunc

telemPropertyFunc :: Parser TelemetryProperty
telemPropertyFunc = Func <$> P.gaspProperty "fn" L.identifier
