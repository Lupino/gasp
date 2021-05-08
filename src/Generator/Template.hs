module Generator.Template
  ( getTemplateFileAbsPath
  , compileAndRenderTemplate
  , compileAndRenderTextTemplate
  ) where

import qualified Data.Aeson           as Aeson
import           Data.Text            (Text)
import qualified Text.Mustache        as Mustache
import           Text.Mustache.Render (SubstitutionError (..))
import           Text.Printf          (printf)

import           Path                 (Abs, Dir, File, Path, Rel, toFilePath,
                                       (</>))

-- | Takes template file path relative to template root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: Path Abs Dir -> Path Rel File -> Path Abs File
getTemplateFileAbsPath tmplDir tmplFilePathInTemplateDir =
  tmplDir </> tmplFilePathInTemplateDir

compileAndRenderTemplate
    :: Path Abs Dir
    -> Path Rel File  -- ^ Path to the template file.
    -> Aeson.Value  -- ^ JSON to be provided as template data.
    -> IO Text
compileAndRenderTemplate tmplDir relTmplPath tmplData =
  (`renderMustacheTemplate` tmplData) <$> compileMustacheTemplate tmplDir relTmplPath

compileMustacheTemplate
    :: Path Abs Dir
    -> Path Rel File  -- ^ Path to the template file.
    -> IO Mustache.Template
compileMustacheTemplate tmplDir relTmplPath = do
    eitherTemplate <- Mustache.automaticCompile [toFilePath templateDirAbsPath]
                                                (toFilePath absTmplPath)
    return $ either raiseCompileError id eitherTemplate
  where
    raiseCompileError err = error $  -- TODO: Handle these errors better?
        printf "Compilation of template %s failed. %s" (show relTmplPath) (show err)

    templateDirAbsPath = tmplDir
    absTmplPath = getTemplateFileAbsPath tmplDir relTmplPath

areAllErrorsSectionDataNotFound :: [SubstitutionError] -> Bool
areAllErrorsSectionDataNotFound = all isSectionDataNotFoundError
  where
    isSectionDataNotFoundError e = case e of
        SectionTargetNotFound _ -> True
        _                       -> False


-- NOTE(matija): Mustache reports errors when object does
-- not have a property specified in the template, which we use to implement
-- conditionals. This is why we ignore these errors.
renderMustacheTemplate :: Mustache.Template -> Aeson.Value -> Text
renderMustacheTemplate mustacheTemplate templateData
  | null errors || areAllErrorsSectionDataNotFound errors = fileText
  | otherwise =
    error $ "Unexpected errors occured while rendering template: "
          ++ show errors


  where (errors, fileText) =
            Mustache.checkedSubstitute mustacheTemplate templateData

compileAndRenderTextTemplate
  :: Text
  -> Aeson.Value  -- ^ JSON to be provided as template data.
  -> Text
compileAndRenderTextTemplate tmplCode tmplData =
  case Mustache.compileTemplate "inline" tmplCode of
    Left err   -> error $ show err
    Right tmpl -> renderMustacheTemplate tmpl tmplData
