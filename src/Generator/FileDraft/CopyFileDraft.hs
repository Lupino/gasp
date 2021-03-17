module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import           Generator.FileDraft.Writeable
import           Generator.FileDraft.WriteableMonad
import           Path                               (Abs, File, Path, Rel,
                                                     parent, toFilePath, (</>))


-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { _dstPath :: !(Path Rel File)-- ^ Path where the file will be copied to.
    , _srcPath :: !(Path Abs File) -- ^ Absolute path of source file to copy.
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (toFilePath $ parent absDraftDstPath)
        copyFile (toFilePath $ _srcPath draft) (toFilePath absDraftDstPath)
      where
          absDraftDstPath = absDstDirPath </> _dstPath draft
