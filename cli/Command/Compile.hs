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
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromJust)
import qualified Lib
import           Path                   (Abs, Dir, File, Path, Rel, parseAbsDir,
                                         toFilePath, (</>))
import qualified Util.IO


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
compileIO gaspProjectDir options = do
    gaspFiles <- findGaspFile gaspProjectDir
    case gaspFiles of
      [] -> return $ Left "No *.gasp file present in the root of Gasp project."
      _  -> Lib.compile gaspFiles options
  where
    findGaspFile :: Path Abs Dir -> IO [Path Abs File]
    findGaspFile dir = do
        (files, _) <- liftIO $ Util.IO.listDirectory dir
        return $ map (dir </>) $ filter isGaspFile files

    isGaspFile :: Path Rel File -> Bool
    isGaspFile path = ".gasp" `isSuffixOf` toFilePath path
                      && (length (toFilePath path) > length (".gasp" :: String))

compileOptions :: CompileType -> [String] -> Command (Path Abs Dir, CompileOptions)
compileOptions ctp argv = do
  gaspProjectDir <- findGaspProjectRootDirFromCwd
  gaspTemplateDir <- findGaspTemplateDir gaspProjectDir
  return (gaspProjectDir , parseCompileOptions CompileOptions
    { externalCodeDirPath = gaspProjectDir </> Common.extCodeDirInGaspProjectDir
    , compileType         = ctp
    , projectRootDir      = gaspProjectDir </> Common.buildGaspDirInGaspProjectDir
    , templateDir         = gaspTemplateDir
    , lowMemory           = False
    , isProd              = False
    } argv)

parseCompileOptions :: CompileOptions -> [String] -> CompileOptions
parseCompileOptions opts []                   = opts
parseCompileOptions opts ("--low-memory":xs)   = parseCompileOptions opts {lowMemory = True} xs
parseCompileOptions opts ("--template":v:xs) = parseCompileOptions opts {templateDir = fromJust $ parseAbsDir v} xs
parseCompileOptions opts ("--production":xs) = parseCompileOptions opts {isProd = True} xs
parseCompileOptions opts (_:xs) = parseCompileOptions opts xs
