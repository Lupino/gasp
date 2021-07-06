module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import           Generator.FileDraft.Writeable
import           Generator.FileDraft.WriteableMonad
import           System.FilePath                    ((</>))
import           Util.IO                            (parent)


-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { _dstPath :: !FilePath -- ^ Path where the file will be copied to.
    , _srcPath :: !FilePath -- ^ Absolute path of source file to copy.
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (parent absDraftDstPath)
        copyFile (_srcPath draft) absDraftDstPath
      where
          absDraftDstPath = absDstDirPath </> _dstPath draft
