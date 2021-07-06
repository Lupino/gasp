module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       ) where

import qualified Data.Aeson                            as Aeson
import qualified Generator.FileDraft.CopyFileDraft     as CopyFD
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import           Generator.FileDraft.Writeable


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


createTemplateFileDraft :: FilePath
                        -> FilePath
                        -> FilePath
                        -> Maybe Aeson.Value
                        -> FileDraft
createTemplateFileDraft dstPath tmplPath tmplSrcPath tmplData =
    FileDraftTemplateFd $ TmplFD.TemplateFileDraft { TmplFD._dstPath = dstPath
                                                   , TmplFD._tmplPath = tmplPath
                                                   , TmplFD._srcPathInTmplDir = tmplSrcPath
                                                   , TmplFD._tmplData = tmplData
                                                   }

createCopyFileDraft :: FilePath -> FilePath -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFD.CopyFileDraft { CopyFD._dstPath = dstPath, CopyFD._srcPath = srcPath}
