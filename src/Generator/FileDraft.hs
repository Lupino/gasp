module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       ) where

import qualified Data.Aeson                            as Aeson
import           Generator.Common                      (ProjectRootDir)
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import           Generator.FileDraft.Writeable
import           Generator.Templates                   (TemplatesDir)
import           StrongPath                            (File, Path, Rel)


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TmplFD.TemplateFileDraft
    deriving (Show, Eq)

instance Writeable FileDraft where
    write dstDir dataDir (FileDraftTemplateFd draft) = write dstDir dataDir draft


createTemplateFileDraft :: Path (Rel ProjectRootDir) File
                        -> Path (Rel TemplatesDir) File
                        -> Maybe Aeson.Value
                        -> FileDraft
createTemplateFileDraft dstPath tmplSrcPath tmplData =
    FileDraftTemplateFd $ TmplFD.TemplateFileDraft { TmplFD._dstPath = dstPath
                                                   , TmplFD._srcPathInTmplDir = tmplSrcPath
                                                   , TmplFD._tmplData = tmplData
                                                   }
