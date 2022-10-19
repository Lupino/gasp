module Generator.AppGenerator
  ( generateApp
  , makeSimpleTemplateFD
  ) where

import           Gasp
import           Generator.AppGenerator.Common   (makeSimpleTemplateFD,
                                                  readTemplateFiles)
import           Generator.ExternalCodeGenerator (generateExternalCodeDir)
import           Generator.FileDraft


generateApp :: (FilePath -> Bool) -> FilePath -> Gasp -> IO [FileDraft]
generateApp filterFun tmplPath gasp = do
  tmplFiles <- filter filterFun <$> readTemplateFiles tmplPath
  return $ map (\v -> makeSimpleTemplateFD v tmplPath gasp) tmplFiles
    ++ generateExternalCodeDir gasp
