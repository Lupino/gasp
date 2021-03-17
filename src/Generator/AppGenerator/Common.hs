module Generator.AppGenerator.Common
    ( makeSimpleTemplateFD
    , readTemplateFiles
    ) where

import qualified Data.Aeson          as Aeson
import           Gasp                (Gasp)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Path                (Abs, Dir, File, Path, Rel)
import           Util.IO             (listDirectoryDeep)

-- * Template

makeSimpleTemplateFD
  :: Path Rel File
  -> Path Abs Dir
  -> Gasp -> FileDraft
makeSimpleTemplateFD srcPath tmplPath gasp = makeTemplateFD srcPath tmplPath srcPath (Just $ Aeson.toJSON gasp)

makeTemplateFD :: Path Rel File
               -> Path Abs Dir
               -> Path Rel File
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath tmplPath relDstPath =
    createTemplateFileDraft relDstPath tmplPath relSrcPath

-- | Returns all files contained in the specified external code dir, recursively.
readTemplateFiles :: Path Abs Dir -> IO [Path Rel File]
readTemplateFiles = listDirectoryDeep
