module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       ) where

import qualified Data.Aeson                            as Aeson
import           Generator.Common                      (ProjectRootDir)
import qualified Generator.FileDraft.CopyFileDraft     as CopyFD
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import           Generator.FileDraft.Writeable
import           Generator.Templates                   (DataDir, TemplatesDir)
import           StrongPath                            (Abs, Dir, File, Path,
                                                        Rel)


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TmplFD.TemplateFileDraft
    | FileDraftCopyFd CopyFD.CopyFileDraft
    deriving (Show, Eq)

instance Writeable FileDraft where
    write dstDir (FileDraftTemplateFd draft) = write dstDir draft
    write dstDir (FileDraftCopyFd draft)     = write dstDir draft


createTemplateFileDraft :: Path (Rel ProjectRootDir) File
                        -> Path Abs (Dir DataDir)
                        -> Path (Rel TemplatesDir) File
                        -> Maybe Aeson.Value
                        -> FileDraft
createTemplateFileDraft dstPath dataPath tmplSrcPath tmplData =
    FileDraftTemplateFd $ TmplFD.TemplateFileDraft { TmplFD._dstPath = dstPath
                                                   , TmplFD._dataPath = dataPath
                                                   , TmplFD._srcPathInTmplDir = tmplSrcPath
                                                   , TmplFD._tmplData = tmplData
                                                   }

createCopyFileDraft :: Path (Rel ProjectRootDir) File -> Path Abs File -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFD.CopyFileDraft { CopyFD._dstPath = dstPath, CopyFD._srcPath = srcPath}
