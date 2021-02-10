module Generator.AppGenerator.Common
    ( makeSimpleTemplateFD
    , readTemplateFiles
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Functor        ((<&>))
import           Gasp                (Gasp)
import           Generator.Common    (ProjectRootDir)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Generator.Template  (TemplateDir)
import           StrongPath          (Abs, Dir, File, Path, Rel)
import qualified StrongPath          as SP
import qualified Util.IO

-- * Template

makeSimpleTemplateFD
  :: Path (Rel TemplateDir) File
  -> Path Abs (Dir TemplateDir)
  -> Gasp -> FileDraft
makeSimpleTemplateFD srcPath tmplPath gasp = makeTemplateFD srcPath tmplPath dstPath (Just $ Aeson.toJSON gasp)
    where dstPath = SP.castRel srcPath :: Path (Rel ProjectRootDir) File

makeTemplateFD :: Path (Rel TemplateDir) File
               -> Path Abs (Dir TemplateDir)
               -> Path (Rel ProjectRootDir) File
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath tmplPath relDstPath =
    createTemplateFileDraft relDstPath tmplPath relSrcPath

-- | Returns all files contained in the specified external code dir, recursively.
readTemplateFiles :: Path Abs (Dir TemplateDir) -> IO [Path (Rel TemplateDir) File]
readTemplateFiles templateDir =
  Util.IO.listDirectoryDeep (SP.toPathAbsDir templateDir) <&> map SP.fromPathRelFile
