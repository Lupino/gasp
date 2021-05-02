module Generator.AppGenerator
  ( generateApp
  , makeSimpleTemplateFD
  ) where

import           Gasp
import           Generator.AppGenerator.Common   (makeSimpleTemplateFD,
                                                  readTemplateFiles)
import           Generator.ExternalCodeGenerator (generateExternalCodeDir)
import           Generator.FileDraft
import           Path                            (Abs, Dir, Path)

generateApp :: Path Abs Dir -> Gasp -> IO [FileDraft]
generateApp tmplPath gasp = do
  tmplFiles <- readTemplateFiles tmplPath
  return $ map (\v -> makeSimpleTemplateFD v tmplPath gasp) tmplFiles
    ++ generateExternalCodeDir gasp
