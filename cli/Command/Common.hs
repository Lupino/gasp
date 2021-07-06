module Command.Common
  ( findGaspProjectRootDirFromCwd
  , findGaspTemplateDir
  , gaspSaysC
  , gaspSays
  , mainGaspFile
  , buildGaspDirInGaspProjectDir
  , extCodeDirInGaspProjectDir
  ) where

import           Command                (Command, CommandError (..))
import           Control.Monad          (unless, when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Paths_gasp
import           System.Directory       (doesFileExist, doesPathExist,
                                         getCurrentDirectory)
import           System.Environment     (lookupEnv)
import           System.FilePath        ((</>))
import           Util.IO                (parent)
import qualified Util.Terminal          as Term

mainGaspFile :: FilePath
mainGaspFile = "main.gasp"

buildGaspDirInGaspProjectDir :: FilePath
buildGaspDirInGaspProjectDir = "build"

extCodeDirInGaspProjectDir :: FilePath
extCodeDirInGaspProjectDir = "src"

findGaspProjectRoot :: FilePath -> Command FilePath
findGaspProjectRoot currentDir = do
  let absCurrentDirFp = currentDir
  doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
  unless doesCurrentDirExist (throwError notFoundError)
  let buildGaspRootFilePath = absCurrentDirFp </> mainGaspFile
  isCurrentDirRoot <- liftIO $ doesFileExist buildGaspRootFilePath
  if isCurrentDirRoot
    then return currentDir
    else do let parentDir = parent currentDir
            when (parentDir == currentDir) (throwError notFoundError)
            findGaspProjectRoot parentDir
  where
    notFoundError = CommandError ("Couldn't find gasp project root - make sure"
                                  ++ " you are running this command from Gasp project.")

findGaspProjectRootDirFromCwd :: Command FilePath
findGaspProjectRootDirFromCwd = do
  absCurrentDir <- liftIO getCurrentDirectory
  findGaspProjectRoot absCurrentDir


findGaspTemplateDir :: FilePath -> Command FilePath
findGaspTemplateDir outDir = do
  menvPath <- liftIO $ lookupEnv "GASP_TEMPLATE_PATH"
  case menvPath of
    Just envPath -> return envPath
    Nothing -> do
      doesTempDirExist <- liftIO $ doesPathExist tempDir
      if doesTempDirExist then return tempDir
                          else liftIO $ (</> "template") <$> Paths_gasp.getDataDir


  where dataDir = outDir
        tempDir = dataDir </> "template"

gaspSaysC :: String -> Command ()
gaspSaysC = liftIO . gaspSays

gaspSays :: String -> IO ()
gaspSays = putStrLn . Term.applyStyles [Term.Yellow]
