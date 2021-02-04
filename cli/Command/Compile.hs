module Command.Compile
    ( compileIO
    , compile
    ) where

import           Command                (Command, CommandError (..))
import           Command.Common         (findGaspDataDir,
                                         findGaspProjectRootDirFromCwd,
                                         gaspSaysC)
import qualified Common
import           CompileOptions         (CompileOptions (..))
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (find, isSuffixOf)
import qualified Lib
import qualified Path                   as P
import           StrongPath             (Abs, Dir, Path, (</>))
import qualified StrongPath             as SP
import qualified Util.IO


compile :: Command ()
compile = do
    gaspProjectDir <- findGaspProjectRootDirFromCwd
    gaspDataDir <- findGaspDataDir gaspProjectDir
    let outDir = gaspProjectDir </> Common.buildGaspDirInGaspProjectDir

    gaspSaysC "Compiling gasp code..."
    compilationResult <- liftIO $ compileIO gaspProjectDir outDir gaspDataDir
    case compilationResult of
        Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
        Right () -> gaspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Gasp source code in gaspProjectDir directory and generates a project
--   in given outDir directory.
compileIO :: Path Abs (Dir Common.GaspProjectDir)
        -> Path Abs (Dir Lib.ProjectRootDir)
        -> Path Abs (Dir Lib.DataDir)
        -> IO (Either String ())
compileIO gaspProjectDir outDir gaspDataDir = do
    maybeGaspFile <- findGaspFile gaspProjectDir
    case maybeGaspFile of
        Nothing -> return $ Left "No *.gasp file present in the root of Gasp project."
        Just gaspFile -> Lib.compile gaspFile outDir gaspDataDir options
  where
    findGaspFile :: Path Abs (Dir d) -> IO (Maybe (Path Abs SP.File))
    findGaspFile dir = do
        (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir dir)
        return $ (dir SP.</>) . SP.fromPathRelFile <$> find isGaspFile files

    isGaspFile :: P.Path P.Rel P.File -> Bool
    isGaspFile path = ".gasp" `isSuffixOf` P.toFilePath path
                      && (length (P.toFilePath path) > length (".gasp" :: String))

    options = CompileOptions
        { externalCodeDirPath = gaspProjectDir </> Common.extCodeDirInGaspProjectDir
        }
