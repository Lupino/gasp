module Generator.Templates
       ( getTemplatesDirAbsPath
       , getTemplateFileAbsPath
       , compileAndRenderTemplate
       , DataDir, TemplatesDir
       ) where

import qualified Data.Aeson           as Aeson
import           Data.Text            (Text)
import qualified Path                 as P
import qualified Text.Mustache        as Mustache
import           Text.Mustache.Render (SubstitutionError (..))
import           Text.Printf          (printf)

import           StrongPath           (Abs, Dir, File, Path, Rel, (</>))
import qualified StrongPath           as SP

-- TODO: Write tests for this file! But first we need to decouple logic from IO
--   so that we can mock it.

data DataDir
data TemplatesDir

-- | Returns absolute path of templates root directory.
getTemplatesDirAbsPath :: Path Abs (Dir DataDir) -> Path Abs (Dir TemplatesDir)
getTemplatesDirAbsPath dataDir = dataDir </> templatesDirPathInDataDir

-- | Takes template file path relative to templates root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: Path Abs (Dir DataDir) -> Path (Rel TemplatesDir) File -> Path Abs File
getTemplateFileAbsPath dataDir tmplFilePathInTemplatesDir =
  absPathOfTemplateFileInDataDir tmplFilePathInDataDir
  where
    tmplFilePathInDataDir :: Path (Rel DataDir) File
    tmplFilePathInDataDir = templatesDirPathInDataDir </> tmplFilePathInTemplatesDir

    absPathOfTemplateFileInDataDir :: Path (Rel DataDir) File -> Path Abs File
    absPathOfTemplateFileInDataDir filePath = (dataDir </> filePath)

templatesDirPathInDataDir :: Path (Rel DataDir) (Dir TemplatesDir)
templatesDirPathInDataDir = SP.fromPathRelDir [P.reldir|templates|]

compileAndRenderTemplate
    :: Path Abs (Dir DataDir)
    -> Path (Rel TemplatesDir) File  -- ^ Path to the template file.
    -> Aeson.Value  -- ^ JSON to be provided as template data.
    -> IO Text
compileAndRenderTemplate dataDir relTmplPath tmplData = do
    mustacheTemplate <- compileMustacheTemplate dataDir relTmplPath
    renderMustacheTemplate mustacheTemplate tmplData

compileMustacheTemplate
    :: Path Abs (Dir DataDir)
    -> Path (Rel TemplatesDir) File  -- ^ Path to the template file.
    -> IO Mustache.Template
compileMustacheTemplate dataDir relTmplPath = do
    eitherTemplate <- Mustache.automaticCompile [SP.toFilePath templatesDirAbsPath]
                                                (SP.toFilePath absTmplPath)
    return $ either raiseCompileError id eitherTemplate
  where
    raiseCompileError err = error $  -- TODO: Handle these errors better?
        printf "Compilation of template %s failed. %s" (show relTmplPath) (show err)

    templatesDirAbsPath = getTemplatesDirAbsPath dataDir
    absTmplPath = getTemplateFileAbsPath dataDir relTmplPath

areAllErrorsSectionDataNotFound :: [SubstitutionError] -> Bool
areAllErrorsSectionDataNotFound subsErrors = all isSectionDataNotFoundError subsErrors
  where
    isSectionDataNotFoundError e = case e of
        SectionTargetNotFound _ -> True
        _                       -> False

renderMustacheTemplate :: Mustache.Template -> Aeson.Value -> IO Text
renderMustacheTemplate mustacheTemplate templateData = do
    let mustacheTemplateData = Mustache.toMustache templateData
    let (errors, fileText) =
            Mustache.checkedSubstituteValue mustacheTemplate mustacheTemplateData

    -- NOTE(matija): Mustache reports errors when object does
    -- not have a property specified in the template, which we use to implement
    -- conditionals. This is why we ignore these errors.
    if (null errors) || (areAllErrorsSectionDataNotFound errors)
        then (return fileText)
        else (error $ "Unexpected errors occured while rendering template: "
            ++ (show errors))
