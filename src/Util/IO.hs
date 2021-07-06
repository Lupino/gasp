module Util.IO
  ( listDirectoryDeep
  , listDirectory
  , parent
  ) where

import           Control.Exception (catch, throw)
import           Control.Monad     (filterM)
import           System.Directory  (doesDirectoryExist, doesFileExist)
import qualified System.Directory  as Dir (listDirectory)
import           System.FilePath   (addTrailingPathSeparator,
                                    dropTrailingPathSeparator, takeBaseName,
                                    takeDirectory, (</>))
import           System.IO.Error   (isDoesNotExistError)

parent :: FilePath -> FilePath
parent = addTrailingPathSeparator . takeDirectory . dropTrailingPathSeparator

dirname :: FilePath -> FilePath
dirname = takeBaseName . dropTrailingPathSeparator


-- TODO: write tests.
-- | Lists all files in the directory recursively.
-- All paths are relative to the directory we are listing.
-- If directory does not exist, returns empty list.
--
-- Example: Imagine we have directory foo that contains test.txt and bar/test2.txt.
-- If we call
-- >>> listDirectoryDeep "foo/"
-- we should get
-- >>> ["test.txt", "bar/text2.txt"]
listDirectoryDeep :: FilePath -> IO [FilePath]
listDirectoryDeep absDirPath = do
    (relFilePaths, relSubDirPaths) <- listDirectory absDirPath
        `catch` \e -> if isDoesNotExistError e then return ([], []) else throw e
    relSubDirFilesPaths <- mapM (listSubDirDeep . (absDirPath </>)) relSubDirPaths
    return $ relFilePaths ++ concat relSubDirFilesPaths
  where
      -- | NOTE: Here, returned paths are relative to the main dir whose sub dir we are listing,
      --   which is one level above what you might intuitively expect.
      listSubDirDeep :: FilePath -> IO [FilePath]
      listSubDirDeep subDirPath = do
          files <- listDirectoryDeep subDirPath
          return $ map (dirname subDirPath </>) files

-- TODO: write tests.
-- | Lists files and directories at top lvl of the directory.
listDirectory :: FilePath -> IO ([FilePath], [FilePath])
listDirectory absDirPath = do
    fpRelItemPaths <- Dir.listDirectory fpAbsDirPath
    relFilePaths <- filterFiles fpAbsDirPath fpRelItemPaths
    relDirPaths <- filterDirs fpAbsDirPath fpRelItemPaths
    return (relFilePaths, relDirPaths)
  where
      fpAbsDirPath :: FilePath
      fpAbsDirPath = absDirPath

      filterFiles :: FilePath -> [FilePath] -> IO [FilePath]
      filterFiles absDir relItems = filterM (doesFileExist . (absDir </>)) relItems

      filterDirs :: FilePath -> [FilePath] -> IO [FilePath]
      filterDirs absDir relItems = filterM (doesDirectoryExist . (absDir </>)) relItems
