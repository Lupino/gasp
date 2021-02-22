module Command.Compile
    ( compileIO
    , compile
    , compileOptions
    ) where

import           Command                (Command, CommandError (..))
import           Command.Common         (findGaspProjectRootDirFromCwd,
                                         findGaspTemplateDir, gaspSaysC)
import qualified Common
import           CompileOptions         (CompileOptions (..))
import           Control.Monad          (unless)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (find, isSuffixOf)
import           Data.Maybe             (fromJust)
import qualified Lib
import qualified Path                   as P
import           StrongPath             (Abs, Dir, Path, (</>))
import qualified StrongPath             as SP
import qualified Util.IO


compile :: Bool -> [String] -> Command ()
compile syntaxTree argv = do
  (gaspProjectDir, options) <- compileOptions syntaxTree argv
  unless syntaxTree $ gaspSaysC "Compiling gasp code..."
  compilationResult <- liftIO $ compileIO gaspProjectDir options
  case compilationResult of
      Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
      Right () -> unless syntaxTree $ gaspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Gasp source code in gaspProjectDir directory and generates a project
--   in given outDir directory.
compileIO :: Path Abs (Dir Common.GaspProjectDir) -> CompileOptions -> IO (Either String ())
compileIO gaspProjectDir options = do
    maybeGaspFile <- findGaspFile gaspProjectDir
    case maybeGaspFile of
        Nothing -> return $ Left "No *.gasp file present in the root of Gasp project."
        Just gaspFile -> Lib.compile gaspFile options
  where
    findGaspFile :: Path Abs (Dir d) -> IO (Maybe (Path Abs SP.File))
    findGaspFile dir = do
        (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir dir)
        return $ (dir SP.</>) . SP.fromPathRelFile <$> find isGaspFile files

    isGaspFile :: P.Path P.Rel P.File -> Bool
    isGaspFile path = ".gasp" `isSuffixOf` P.toFilePath path
                      && (length (P.toFilePath path) > length (".gasp" :: String))

compileOptions :: Bool -> [String] -> Command (Path Abs (Dir Common.GaspProjectDir), CompileOptions)
compileOptions syntaxTree argv = do
  gaspProjectDir <- findGaspProjectRootDirFromCwd
  gaspTemplateDir <- findGaspTemplateDir gaspProjectDir
  return (gaspProjectDir , parseCompileOptions CompileOptions
    { externalCodeDirPath = gaspProjectDir </> Common.extCodeDirInGaspProjectDir
    , showSyntaxTree = syntaxTree
    , projectRootDir = gaspProjectDir </> Common.buildGaspDirInGaspProjectDir
    , templateDir    = gaspTemplateDir
    , lowMemory      = False
    } argv)

parseCompileOptions :: CompileOptions -> [String] -> CompileOptions
parseCompileOptions opts []                   = opts
parseCompileOptions opts ("--low-memory":xs)   = parseCompileOptions opts {lowMemory = True} xs
parseCompileOptions opts ("--template":v:xs) = parseCompileOptions opts {templateDir = fromJust $ SP.parseAbsDir v} xs
parseCompileOptions opts (_:xs) = parseCompileOptions opts xs
