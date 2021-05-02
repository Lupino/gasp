module Parser
    ( parseGasp
    ) where

import qualified Gasp
import           Lexer
import           Parser.AGpio       (agpio)
import           Parser.App         (app)
import           Parser.Attr        (attr)
import           Parser.Command     (command)
import           Parser.Common      (runGaspParser)
import           Parser.Constant    (constant)
import           Parser.Every       (every)
import           Parser.Function    (function)
import           Parser.Gpio        (gpio)
import           Parser.Import      (importParser)
import           Parser.Init        (initP)
import           Parser.Loop        (loop)
import           Parser.Metric      (metric)
import           Parser.Require     (require)
import           Parser.Rule        (rule)
import           Parser.Setup       (setup)
import           Parser.Timer       (timer)
import           Parser.Uart        (uart)
import           Text.Parsec        (ParseError, eof, many1, (<|>))
import           Text.Parsec.String (Parser)

expr :: Parser Gasp.Expr
expr
    =   exprApp
    <|> exprCmd
    <|> exprFunction
    <|> exprLoop
    <|> exprSetup
    <|> exprInit
    <|> exprAttr
    <|> exprMetric
    <|> exprEvery
    <|> exprGpio
    <|> exprAGpio
    <|> exprUart
    <|> exprRule
    <|> exprConst
    <|> exprRequire
    <|> exprImport
    <|> exprTimer

exprApp :: Parser Gasp.Expr
exprApp = Gasp.ExprApp <$> app

exprCmd :: Parser Gasp.Expr
exprCmd = Gasp.ExprCmd <$> command

exprFunction :: Parser Gasp.Expr
exprFunction = Gasp.ExprFunction <$> function

exprLoop :: Parser Gasp.Expr
exprLoop = Gasp.ExprLoop <$> loop

exprSetup :: Parser Gasp.Expr
exprSetup = Gasp.ExprSetup <$> setup

exprInit :: Parser Gasp.Expr
exprInit = Gasp.ExprInit <$> initP

exprAttr :: Parser Gasp.Expr
exprAttr = Gasp.ExprAttr <$> attr

exprMetric :: Parser Gasp.Expr
exprMetric = Gasp.ExprMetric <$> metric

exprEvery :: Parser Gasp.Expr
exprEvery = Gasp.ExprEvery <$> every

exprGpio :: Parser Gasp.Expr
exprGpio = Gasp.ExprGpio <$> gpio

exprAGpio :: Parser Gasp.Expr
exprAGpio = Gasp.ExprAGpio <$> agpio

exprUart :: Parser Gasp.Expr
exprUart = Gasp.ExprUart <$> uart

exprRule :: Parser Gasp.Expr
exprRule = Gasp.ExprRule <$> rule

exprConst :: Parser Gasp.Expr
exprConst = Gasp.ExprConst <$> constant

exprRequire :: Parser Gasp.Expr
exprRequire = Gasp.ExprRequire <$> require

exprImport :: Parser Gasp.Expr
exprImport = Gasp.ExprImport <$> importParser

exprTimer :: Parser Gasp.Expr
exprTimer = Gasp.ExprTimer <$> timer

-- | Top level parser, produces Gasp.
gaspParser :: Parser Gasp.Gasp
gaspParser = do
    -- NOTE(matija): this is the only place we need to use whiteSpace, to skip empty lines
    -- and comments in the beginning of file. All other used parsers are lexeme parsers
    -- so they do it themselves.
    whiteSpace

    exprs <- many1 expr

    eof

    -- TODO(matija): after we parsed everything, we should do semantic analysis
    -- e.g. check there is only 1 title - if not, throw a meaningful error.
    -- Also, check there is at least one Page defined.

    return $ Gasp.fromGaspExprs exprs

-- | Top level parser executor.
parseGasp :: FilePath -> IO (Either ParseError Gasp.Gasp)
parseGasp = runGaspParser gaspParser
