module Generator.AppGenerator.Common
    ( makeSimpleTemplateFD
    , asTmplFile
    , appRootDirInProjectRootDir
    , extCodeDirInProjectRootDir
    ) where

import qualified Data.Aeson                             as Aeson
import qualified Path                                   as P

import           Gasp                                   (Gasp)
import           Generator.Common                       (ProjectRootDir)
import           Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import           Generator.FileDraft                    (FileDraft,
                                                         createTemplateFileDraft)
import           Generator.Templates                    (DataDir, TemplatesDir)
import           StrongPath                             (Abs, Dir, File, Path,
                                                         Rel, (</>))
import qualified StrongPath                             as SP


data AppRootDir
data AppTemplatesDir


asTmplFile :: P.Path P.Rel P.File -> Path (Rel AppTemplatesDir) File
asTmplFile = SP.fromPathRelFile

-- * Paths

-- | Path where app root dir is generated.
appRootDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir AppRootDir)
appRootDirInProjectRootDir = SP.fromPathRelDir [P.reldir|app|]

-- * Templates

makeSimpleTemplateFD
  :: Path (Rel AppTemplatesDir) File
  -> Path Abs (Dir DataDir)
  -> Gasp -> FileDraft
makeSimpleTemplateFD srcPath dataPath gasp = makeTemplateFD srcPath dataPath dstPath (Just $ Aeson.toJSON gasp)
    where dstPath = (SP.castRel srcPath) :: Path (Rel AppRootDir) File

makeTemplateFD :: Path (Rel AppTemplatesDir) File
               -> Path Abs (Dir DataDir)
               -> Path (Rel AppRootDir) File
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath dataPath relDstPath tmplData =
    createTemplateFileDraft
        (appRootDirInProjectRootDir </> relDstPath)
        dataPath
        (appTemplatesDirInTemplatesDir </> relSrcPath)
        tmplData

-- | Path where app app templates reside.
appTemplatesDirInTemplatesDir :: Path (Rel TemplatesDir) (Dir AppTemplatesDir)
appTemplatesDirInTemplatesDir = SP.fromPathRelDir [P.reldir|app|]

extCodeDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir GeneratedExternalCodeDir)
extCodeDirInProjectRootDir = SP.fromPathRelDir [P.reldir|app|]
