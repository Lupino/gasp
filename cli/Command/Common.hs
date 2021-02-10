module Command.Common
    ( findGaspProjectRootDirFromCwd
    , findGaspProjectRoot
    , findGaspTemplatesDir
    , gaspSaysC
    ) where

import           Control.Monad          (unless, when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import qualified Paths_gasp
import           System.Directory       (doesFileExist, doesPathExist,
                                         getCurrentDirectory)
import qualified System.FilePath        as FP

import           Command                (Command, CommandError (..))
import           Common                 (GaspProjectDir,
                                         buildGaspRootFileInGaspProjectDir,
                                         gaspSays)
import           Lib                    (TemplatesDir)
import           StrongPath             (Abs, Dir, Path)
import qualified StrongPath             as SP


findGaspProjectRoot :: Path Abs (Dir ()) -> Command (Path Abs (Dir GaspProjectDir))
findGaspProjectRoot currentDir = do
    let absCurrentDirFp = SP.toFilePath currentDir
    doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
    unless doesCurrentDirExist (throwError notFoundError)
    let buildGaspRootFilePath = absCurrentDirFp FP.</> SP.toFilePath buildGaspRootFileInGaspProjectDir
    isCurrentDirRoot <- liftIO $ doesFileExist buildGaspRootFilePath
    if isCurrentDirRoot
        then return $ SP.castDir currentDir
        else do let parentDir = SP.parent currentDir
                when (parentDir == currentDir) (throwError notFoundError)
                findGaspProjectRoot parentDir
  where
      notFoundError = CommandError ("Couldn't find gasp project root - make sure"
                                    ++ " you are running this command from Gasp project.")

findGaspProjectRootDirFromCwd :: Command (Path Abs (Dir GaspProjectDir))
findGaspProjectRootDirFromCwd = do
    absCurrentDir <- liftIO getCurrentDirectory
    findGaspProjectRoot (fromJust $ SP.parseAbsDir absCurrentDir)


findGaspTemplatesDir :: Path Abs (Dir GaspProjectDir) -> Command (Path Abs (Dir TemplatesDir))
findGaspTemplatesDir outDir = do
    doesTempDirExist <- liftIO $ doesPathExist tempDir
    if doesTempDirExist then return $ fromJust $ SP.parseAbsDir tempDir
                        else liftIO $ (FP.</> "templates") <$> Paths_gasp.getDataDir >>= SP.parseAbsDir


  where dataDir = SP.toFilePath outDir
        tempDir = dataDir FP.</> "templates"

gaspSaysC :: String -> Command ()
gaspSaysC = liftIO . gaspSays
