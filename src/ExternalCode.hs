module ExternalCode
    ( File
    , filePathInExtCodeDir
    , fileAbsPath
    , fileText
    , readFiles
    ) where

import           Data.Text         (Text)
import qualified Data.Text.Lazy    as TextL
import qualified Data.Text.Lazy.IO as TextL.IO
import           System.FilePath   ((</>))
import qualified Util.IO

data File = File
    { _pathInExtCodeDir :: !FilePath
    , _extCodeDirPath   :: !FilePath
    , _text             :: TextL.Text  -- ^ File content. It will throw error when evaluated if file is not textual file.
    }

instance Show File where
    show = show . _pathInExtCodeDir

instance Eq File where
    f1 == f2 = _pathInExtCodeDir f1 == _pathInExtCodeDir f2

-- | Returns path relative to the external code directory.
filePathInExtCodeDir :: File -> FilePath
filePathInExtCodeDir = _pathInExtCodeDir

-- | Unsafe method: throws error if text could not be read (if file is not a textual file)!
fileText :: File -> Text
fileText = TextL.toStrict . _text

-- | Returns absolute path of the external code file.
fileAbsPath :: File -> FilePath
fileAbsPath file = _extCodeDirPath file </> _pathInExtCodeDir file

-- | Returns all files contained in the specified external code dir, recursively.
readFiles :: FilePath -> IO [File]
readFiles extCodeDirPath = do
    relFilePaths <- Util.IO.listDirectoryDeep extCodeDirPath
    let absFilePaths = map (extCodeDirPath </>) relFilePaths
    -- NOTE: We read text from all the files, regardless if they are text files or not, because
    --   we don't know if they are a text file or not.
    --   Since we do lazy reading (Text.Lazy), this is not a problem as long as we don't try to use
    --   text of a file that is actually not a text file -> then we will get an error when Haskell
    --   actually tries to read that file.
    -- TODO: We are doing lazy IO here, and there is an idea of it being a thing to avoid, due to no
    --   control over when resources are released and similar.
    --   If we do figure out that this is causing us problems, we could do the following refactoring:
    --     Don't read files at this point, just list them, and Gasp will contain just list of filepaths.
    --     Modify TextFileDraft so that it also takes text transformation function (Text -> Text),
    --     or create new file draft that will support that.
    --     In generator, when creating TextFileDraft, give it function/logic for text transformation,
    --     and it will be taken care of when draft will be written to the disk.
    fileTexts <- mapM TextL.IO.readFile absFilePaths
    let files = zipWith (`File` extCodeDirPath) relFilePaths fileTexts
    return files

