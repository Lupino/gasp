module Command.Compile
    ( compileIO
    , compile
    , compileOptions
    ) where

import           Command                (Command, CommandError (..))
import           Command.Common         (findGaspProjectRootDirFromCwd,
                                         findGaspTemplateDir, gaspSaysC)
import qualified Common
import           CompileOptions         (CompileOptions (..), CompileType,
                                         isCompile)
import           Control.Monad          (when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Data.Text              (pack)
import           Gasp.Flag
import qualified Lib
import           Path                   (Abs, Dir, Path, parseAbsDir, (</>))


compile :: CompileType -> [String] -> Command ()
compile ctp argv = do
  (gaspProjectDir, options) <- compileOptions ctp argv
  when (isCompile ctp) $ gaspSaysC "Compiling gasp code..."
  compilationResult <- liftIO $ compileIO gaspProjectDir options
  case compilationResult of
      Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
      Right () -> when (isCompile ctp) $ gaspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Gasp source code in gaspProjectDir directory and generates a project
--   in given outDir directory.
compileIO :: Path Abs Dir -> CompileOptions -> IO (Either String ())
compileIO gaspProjectDir = Lib.compile gaspFile
  where gaspFile = gaspProjectDir </> Common.mainGaspFile

compileOptions :: CompileType -> [String] -> Command (Path Abs Dir, CompileOptions)
compileOptions ctp argv = do
  gaspProjectDir <- findGaspProjectRootDirFromCwd
  gaspTemplateDir <- findGaspTemplateDir gaspProjectDir
  return (gaspProjectDir , parseCompileOptions CompileOptions
    { externalCodeDirPath = gaspProjectDir </> Common.extCodeDirInGaspProjectDir
    , compileType         = ctp
    , projectRootDir      = gaspProjectDir </> Common.buildGaspDirInGaspProjectDir
    , templateDir         = gaspTemplateDir
    , isProd              = False
    , argvFlags           = []
    } argv)

parseCompileOptions :: CompileOptions -> [String] -> CompileOptions
parseCompileOptions opts []                   = opts
parseCompileOptions opts ("--template":v:xs) = parseCompileOptions opts {templateDir = fromJust $ parseAbsDir v} xs
parseCompileOptions opts ("--production":xs) = parseCompileOptions opts {isProd = True} xs
parseCompileOptions opts ("--flag":k:v:xs)   = parseCompileOptions opts {argvFlags = Flag (pack k) (read v) : argvFlags opts} xs
parseCompileOptions opts (_:xs) = parseCompileOptions opts xs
