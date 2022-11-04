module Parser
  ( parseGasp
  , parseGasp0
  ) where


import           Control.Monad.Except   (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Gasp                   (Expr (..), Gasp, fromGaspExprs)
import           Gasp.Block             (Require (..))
import           Lexer
import           Parser.AGpio           (agpio)
import           Parser.App             (app)
import           Parser.Attr            (attr)
import           Parser.Block
import           Parser.Command         (command)
import           Parser.Common          (runGaspParser)
import           Parser.Constant        (constant)
import           Parser.Every           (every)
import           Parser.Function        (function)
import           Parser.Gpio            (gpio)
import           Parser.Metric          (metric)
import           Parser.Rule            (rule)
import           Parser.Timer           (timer)
import           Parser.Linkage           (linkage)
import           Parser.Uart            (uart)
import           System.Directory       (canonicalizePath, doesFileExist)
import           System.FilePath        (addExtension, hasExtension, (</>))
import           Text.Parsec            (ParseError, eof, many1, parse, (<|>))
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
    <|> exprData
    <|> exprTmpl
    <|> exprRender
    <|> exprRender1
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
    <|> exprLinkage
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

exprData :: Parser Expr
exprData = ExprData <$> data_

exprTmpl :: Parser Expr
exprTmpl = ExprTmpl <$> tmpl

exprRender :: Parser Expr
exprRender = ExprRender <$> render

exprRender1 :: Parser Expr
exprRender1 = ExprRender1 <$> render1

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
exprImport = ExprImport <$> import_

exprTimer :: Parser Expr
exprTimer = ExprTimer <$> timer

exprLinkage :: Parser Expr
exprLinkage = ExprLinkage <$> linkage

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

searchGaspFilePath :: FilePath -> FilePath -> IO (Bool, FilePath)
searchGaspFilePath rootDir path = do
  fp0 <- liftIO . canonicalizePath $ rootDir </> path
  exist <- liftIO $ doesFileExist fp0
  if exist
    then return (True, fp0)
    else
      if hasExtension path
        then return (False, path)
        else searchGaspFilePath rootDir $ addExtension path ".gasp"

scanGaspFile :: [FilePath] -> FilePath -> FilePath -> IO (Bool, FilePath)
scanGaspFile _ rootDir path@('.':_) = searchGaspFilePath rootDir path
scanGaspFile _ rootDir path@('/':_) = searchGaspFilePath rootDir path
scanGaspFile [] _ path = return (False, path)
scanGaspFile (x:xs) rootDir path = do
  (exist, newPath) <- searchGaspFilePath x path
  if exist then return (True, newPath)
           else scanGaspFile xs rootDir path

scanGaspFileAndParse
  :: IORef [FilePath] -> FilePath -> FilePath -> FilePath -> GaspParser [Expr]
scanGaspFileAndParse parsedH tempDir rootDir path = do
  (_, fp) <- liftIO $ scanGaspFile [tempDir] rootDir path

  parsed <- liftIO $ readIORef parsedH
  if fp `elem` parsed
    then return []
    else do
      expr0 <- parseFile fp
      liftIO $! writeIORef parsedH $ fp : parsed

      parseWithRequired parsedH tempDir (parent fp) expr0


parseWithRequired :: IORef [FilePath] -> FilePath -> FilePath -> [Expr] -> GaspParser [Expr]
parseWithRequired _ _ _ [] = return []
parseWithRequired parsedH tempDir rootDir (ExprRequire (Require path) : xs) = do
  expr1 <- scanGaspFileAndParse parsedH tempDir rootDir path
  expr2 <- parseWithRequired parsedH tempDir rootDir xs
  return $ expr1 ++ expr2

parseWithRequired parsedH tempDir rootDir (x : xs) =
  (x:) <$> parseWithRequired parsedH tempDir rootDir xs

parseExpr :: FilePath -> FilePath -> GaspParser [Expr]
parseExpr tempDir fp = do
  parsedH <- liftIO $ newIORef []
  parseWithRequired parsedH tempDir (parent fp) =<< parseFile fp

parseGasp :: FilePath -> FilePath -> IO (Either ParseError Gasp)
parseGasp tempDir fp = runExceptT $ fromGaspExprs <$> parseExpr tempDir fp

parseGasp0 :: String -> String -> Either ParseError [Expr]
parseGasp0 = parse gaspParser
