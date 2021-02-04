module Parser
    ( parseGasp
    ) where

import qualified Gasp
import           Lexer
import           Parser.App         (app)
import           Parser.Attr        (attr)
import           Parser.Command     (command)
import           Parser.Common      (runGaspParser)
import           Parser.Every       (every)
import           Parser.Function    (function)
import           Parser.Gpio        (gpio)
import           Parser.Init        (initP)
import           Parser.Loop        (loop)
import           Parser.Metric      (metric)
import           Parser.Setup       (setup)
import           Parser.Telemetry   (telemetry)
import           Text.Parsec        (ParseError, eof, many1, (<|>))
import           Text.Parsec.String (Parser)

gaspElement :: Parser Gasp.GaspElement
gaspElement
    =   gaspElementApp
    <|> gaspElementCmd
    <|> gaspElementTelemetry
    <|> gaspElementFunction
    <|> gaspElementLoop
    <|> gaspElementSetup
    <|> gaspElementInit
    <|> gaspElementAttr
    <|> gaspElementMetric
    <|> gaspElementEvery
    <|> gaspElementGpio

gaspElementApp :: Parser Gasp.GaspElement
gaspElementApp = Gasp.GaspElementApp <$> app

gaspElementCmd :: Parser Gasp.GaspElement
gaspElementCmd = Gasp.GaspElementCmd <$> command

gaspElementTelemetry :: Parser Gasp.GaspElement
gaspElementTelemetry = Gasp.GaspElementTelemetry <$> telemetry

gaspElementFunction :: Parser Gasp.GaspElement
gaspElementFunction = Gasp.GaspElementFunction <$> function

gaspElementLoop :: Parser Gasp.GaspElement
gaspElementLoop = Gasp.GaspElementLoop <$> loop

gaspElementSetup :: Parser Gasp.GaspElement
gaspElementSetup = Gasp.GaspElementSetup <$> setup

gaspElementInit :: Parser Gasp.GaspElement
gaspElementInit = Gasp.GaspElementInit <$> initP

gaspElementAttr :: Parser Gasp.GaspElement
gaspElementAttr = Gasp.GaspElementAttr <$> attr

gaspElementMetric :: Parser Gasp.GaspElement
gaspElementMetric = Gasp.GaspElementMetric <$> metric

gaspElementEvery :: Parser Gasp.GaspElement
gaspElementEvery = Gasp.GaspElementEvery <$> every

gaspElementGpio :: Parser Gasp.GaspElement
gaspElementGpio = Gasp.GaspElementGpio <$> gpio


-- | Top level parser, produces Gasp.
gaspParser :: Parser Gasp.Gasp
gaspParser = do
    -- NOTE(matija): this is the only place we need to use whiteSpace, to skip empty lines
    -- and comments in the beginning of file. All other used parsers are lexeme parsers
    -- so they do it themselves.
    whiteSpace

    gaspElems <- many1 gaspElement

    eof

    -- TODO(matija): after we parsed everything, we should do semantic analysis
    -- e.g. check there is only 1 title - if not, throw a meaningful error.
    -- Also, check there is at least one Page defined.

    return $ Gasp.fromGaspElems gaspElems

-- | Top level parser executor.
parseGasp :: String -> Either ParseError Gasp.Gasp
parseGasp = runGaspParser gaspParser
