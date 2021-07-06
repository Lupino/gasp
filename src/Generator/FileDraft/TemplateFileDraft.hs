module Generator.FileDraft.TemplateFileDraft
  ( TemplateFileDraft(..)
  ) where

import qualified Data.Aeson                         as Aeson
import           Generator.FileDraft.Writeable
import           Generator.FileDraft.WriteableMonad
import           Generator.Template                 (getTemplateFileAbsPath)
import           System.FilePath                    ((</>))
import           Util.IO                            (parent)

-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { _dstPath          :: !FilePath -- ^ Path where file will be generated.
    , _tmplPath         :: !FilePath
    , _srcPathInTmplDir :: !FilePath -- ^ Path of template source file.
    , _tmplData         :: Maybe Aeson.Value -- ^ Data to be fed to the template while rendering it.

    }
    deriving (Show, Eq)

instance Writeable TemplateFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (parent absDraftDstPath)
        case _tmplData draft of
            Nothing ->
                copyFile absDraftSrcPath absDraftDstPath
            Just tmplData -> do
                content <- compileAndRenderTemplate (_tmplPath draft) (_srcPathInTmplDir draft) tmplData
                writeFileFromText absDraftDstPath content
      where
        absDraftDstPath :: FilePath
        absDraftDstPath = absDstDirPath </> _dstPath draft

        absDraftSrcPath = getTemplateFileAbsPath (_tmplPath draft) (_srcPathInTmplDir draft)
