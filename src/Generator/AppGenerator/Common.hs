module Generator.AppGenerator.Common
    ( makeSimpleTemplateFD
    , readTemplateFiles
    ) where

import qualified Data.Aeson          as Aeson
import           Gasp                (Gasp)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Util.IO             (listDirectoryDeep)

-- * Template

makeSimpleTemplateFD
  :: FilePath
  -> FilePath
  -> Gasp -> FileDraft
makeSimpleTemplateFD srcPath tmplPath gasp = makeTemplateFD srcPath tmplPath srcPath (Just $ Aeson.toJSON gasp)

makeTemplateFD :: FilePath
               -> FilePath
               -> FilePath
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath tmplPath relDstPath =
    createTemplateFileDraft relDstPath tmplPath relSrcPath

-- | Returns all files contained in the specified external code dir, recursively.
readTemplateFiles :: FilePath -> IO [FilePath]
readTemplateFiles = listDirectoryDeep
