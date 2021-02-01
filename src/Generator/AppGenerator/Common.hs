module Generator.AppGenerator.Common
    ( makeSimpleTemplateFD
    , asTmplFile
    , appRootDirInProjectRootDir
    ) where

import qualified Data.Aeson          as Aeson
import qualified Path                as P

import           Gasp                (Gasp)
import           Generator.Common    (ProjectRootDir)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Generator.Templates (TemplatesDir)
import           StrongPath          (Dir, File, Path, Rel, (</>))
import qualified StrongPath          as SP


data AppRootDir
data AppTemplatesDir


asTmplFile :: P.Path P.Rel P.File -> Path (Rel AppTemplatesDir) File
asTmplFile = SP.fromPathRelFile

-- * Paths

-- | Path where app root dir is generated.
appRootDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir AppRootDir)
appRootDirInProjectRootDir = SP.fromPathRelDir [P.reldir|app|]

-- * Templates

makeSimpleTemplateFD :: Path (Rel AppTemplatesDir) File -> Gasp -> FileDraft
makeSimpleTemplateFD srcPath gasp = makeTemplateFD srcPath dstPath (Just $ Aeson.toJSON gasp)
    where dstPath = (SP.castRel srcPath) :: Path (Rel AppRootDir) File

makeTemplateFD :: Path (Rel AppTemplatesDir) File
               -> Path (Rel AppRootDir) File
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath relDstPath tmplData =
    createTemplateFileDraft
        (appRootDirInProjectRootDir </> relDstPath)
        (appTemplatesDirInTemplatesDir </> relSrcPath)
        tmplData

-- | Path where app app templates reside.
appTemplatesDirInTemplatesDir :: Path (Rel TemplatesDir) (Dir AppTemplatesDir)
appTemplatesDirInTemplatesDir = SP.fromPathRelDir [P.reldir|app|]
