module Command.Clean
    ( clean
    ) where

import           Control.Monad.IO.Class (liftIO)
import           System.Directory       (doesDirectoryExist,
                                         removeDirectoryRecursive)
import           System.IO              (hFlush, stdout)

import           Command                (Command)
import           Command.Common         (buildGaspDirInGaspProjectDir,
                                         findGaspProjectRootDirFromCwd)
import           System.FilePath        ((</>))

clean :: Command ()
clean = do
    gaspProjectDir <- findGaspProjectRootDirFromCwd
    let buildGaspDirFp = gaspProjectDir </> buildGaspDirInGaspProjectDir
    liftIO $ putStrLn "Deleting build/ directory..." >> hFlush stdout
    doesDotGaspDirExist <- liftIO $ doesDirectoryExist buildGaspDirFp
    if doesDotGaspDirExist
        then liftIO $ do removeDirectoryRecursive buildGaspDirFp
                         putStrLn "Deleted build/ directory."
        else liftIO $ putStrLn "Nothing to delete: build directory does not exist."

