module Parser
  ( parseGasp
  ) where


import           Control.Monad.Except   (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Gasp                   (Expr (..), Gasp, Require (..),
                                         fromGaspExprs)
import           Lexer
import           Parser.AGpio           (agpio)
import           Parser.App             (app)
import           Parser.Attr            (attr)
import           Parser.Block           (loop, raw, setup)
import           Parser.Command         (command)
import           Parser.Common          (runGaspParser)
import           Parser.Constant        (constant)
import           Parser.Every           (every)
import           Parser.Flag            (flag)
import           Parser.Function        (function)
import           Parser.Gpio            (gpio)
import           Parser.Import          (importParser)
import           Parser.Metric          (metric)
import           Parser.Require         (require)
import           Parser.Rule            (rule)
import           Parser.Timer           (timer)
import           Parser.Uart            (uart)
import           System.Directory       (canonicalizePath, doesFileExist)
import           System.FilePath        (addExtension, (</>))
import           Text.Parsec            (ParseError, eof, many1, (<|>))
import           Text.Parsec.String     (Parser)
import           Util.IO                (parent)

expr :: Parser Expr
expr
    =   exprApp
    <|> exprCmd
    <|> exprFunction
    <|> exprLoop
    <|> exprSetup
    <|> exprRaw
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
    <|> exprFlag

exprApp :: Parser Expr
exprApp = ExprApp <$> app

exprCmd :: Parser Expr
exprCmd = ExprCmd <$> command

exprFunction :: Parser Expr
exprFunction = ExprFunction <$> function

exprLoop :: Parser Expr
exprLoop = ExprLoop <$> loop

exprSetup :: Parser Expr
exprSetup = ExprSetup <$> setup

exprRaw :: Parser Expr
exprRaw = ExprRaw <$> raw

exprAttr :: Parser Expr
exprAttr = ExprAttr <$> attr

exprMetric :: Parser Expr
exprMetric = ExprMetric <$> metric

exprEvery :: Parser Expr
exprEvery = ExprEvery <$> every

exprGpio :: Parser Expr
exprGpio = ExprGpio <$> gpio

exprAGpio :: Parser Expr
exprAGpio = ExprAGpio <$> agpio

exprUart :: Parser Expr
exprUart = ExprUart <$> uart

exprRule :: Parser Expr
exprRule = ExprRule <$> rule

exprConst :: Parser Expr
exprConst = ExprConst <$> constant

exprRequire :: Parser Expr
exprRequire = ExprRequire <$> require

exprImport :: Parser Expr
exprImport = ExprImport <$> importParser

exprTimer :: Parser Expr
exprTimer = ExprTimer <$> timer

exprFlag :: Parser Expr
exprFlag = ExprFlag <$> flag

-- | Top level parser, produces Expr.
gaspParser :: Parser [Expr]
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

    return exprs

type GaspParser = ExceptT ParseError IO

parseFile :: FilePath -> GaspParser [Expr]
parseFile = ExceptT . runGaspParser gaspParser

parseWithRequired :: FilePath -> [Expr] -> GaspParser [Expr]
parseWithRequired _ [] = return []
parseWithRequired rootDir (ExprRequire (Require path) : xs) = do
  fp0 <- liftIO . canonicalizePath $ rootDir </> path
  exist <- liftIO $ doesFileExist fp0
  let fp = if exist then fp0 else addExtension fp0 ".gasp"
  expr0 <- parseFile fp
  expr1 <- parseWithRequired (parent fp) expr0
  expr2 <- parseWithRequired rootDir xs
  return $ expr1 ++ expr2

parseWithRequired rootDir (x : xs) = (x:) <$> parseWithRequired rootDir xs

parseExpr :: FilePath -> GaspParser [Expr]
parseExpr fp = parseWithRequired (parent fp) =<< parseFile fp

parseGasp :: FilePath -> IO (Either ParseError Gasp)
parseGasp fp = runExceptT $ fromGaspExprs <$> parseExpr fp
