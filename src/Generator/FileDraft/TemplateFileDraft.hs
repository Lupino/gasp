module Generator.FileDraft.TemplateFileDraft
  ( TemplateFileDraft(..)
  ) where

import qualified Data.Aeson                         as Aeson

import           Generator.FileDraft.Writeable
import           Generator.FileDraft.WriteableMonad
import           Generator.Template                 (getTemplateFileAbsPath)
import           Path                               (Abs, Dir, File, Path, Rel,
                                                     parent, toFilePath, (</>))

-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { _dstPath          :: !(Path Rel File) -- ^ Path where file will be generated.
    , _tmplPath         :: !(Path Abs Dir)
    , _srcPathInTmplDir :: !(Path Rel File) -- ^ Path of template source file.
    , _tmplData         :: Maybe Aeson.Value -- ^ Data to be fed to the template while rendering it.

    }
    deriving (Show, Eq)

instance Writeable TemplateFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (toFilePath $ parent absDraftDstPath)
        case _tmplData draft of
            Nothing ->
                copyFile (toFilePath absDraftSrcPath) (toFilePath absDraftDstPath)
            Just tmplData -> do
                content <- compileAndRenderTemplate (_tmplPath draft) (_srcPathInTmplDir draft) tmplData
                writeFileFromText (toFilePath absDraftDstPath) content
      where
        absDraftDstPath :: Path Abs File
        absDraftDstPath = absDstDirPath </> _dstPath draft

        absDraftSrcPath = getTemplateFileAbsPath (_tmplPath draft) (_srcPathInTmplDir draft)
