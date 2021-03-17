module Command.Common
    ( findGaspProjectRootDirFromCwd
    , findGaspProjectRoot
    , findGaspTemplateDir
    , gaspSaysC
    ) where

import           Command                (Command, CommandError (..))
import           Common                 (buildGaspRootFileInGaspProjectDir,
                                         gaspSays)
import           Control.Monad          (unless, when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Path                   (Abs, Dir, Path, parent, parseAbsDir,
                                         toFilePath)
import qualified Paths_gasp
import           System.Directory       (doesFileExist, doesPathExist,
                                         getCurrentDirectory)
import           System.Environment     (lookupEnv)
import qualified System.FilePath        as FP


findGaspProjectRoot :: Path Abs Dir -> Command (Path Abs Dir)
findGaspProjectRoot currentDir = do
    let absCurrentDirFp = toFilePath currentDir
    doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
    unless doesCurrentDirExist (throwError notFoundError)
    let buildGaspRootFilePath = absCurrentDirFp FP.</> toFilePath buildGaspRootFileInGaspProjectDir
    isCurrentDirRoot <- liftIO $ doesFileExist buildGaspRootFilePath
    if isCurrentDirRoot
        then return currentDir
        else do let parentDir = parent currentDir
                when (parentDir == currentDir) (throwError notFoundError)
                findGaspProjectRoot parentDir
  where
      notFoundError = CommandError ("Couldn't find gasp project root - make sure"
                                    ++ " you are running this command from Gasp project.")

findGaspProjectRootDirFromCwd :: Command (Path Abs Dir)
findGaspProjectRootDirFromCwd = do
    absCurrentDir <- liftIO getCurrentDirectory
    findGaspProjectRoot (fromJust $ parseAbsDir absCurrentDir)


findGaspTemplateDir :: Path Abs Dir -> Command (Path Abs Dir)
findGaspTemplateDir outDir = do
  menvPath <- liftIO $ lookupEnv "GASP_TEMPLATE_PATH"
  case menvPath of
    Just envPath -> return $ fromJust $ parseAbsDir envPath
    Nothing -> do
      doesTempDirExist <- liftIO $ doesPathExist tempDir
      if doesTempDirExist then return $ fromJust $ parseAbsDir tempDir
                          else liftIO $ (FP.</> "template") <$> Paths_gasp.getDataDir >>= parseAbsDir


  where dataDir = toFilePath outDir
        tempDir = dataDir FP.</> "template"

gaspSaysC :: String -> Command ()
gaspSaysC = liftIO . gaspSays
