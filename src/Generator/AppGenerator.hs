module Generator.AppGenerator
       ( generateApp
       ) where

import           Gasp
import           Generator.AppGenerator.Common   (makeSimpleTemplateFD,
                                                  readTemplateFiles)
import           Generator.ExternalCodeGenerator (generateExternalCodeDir)
import           Generator.FileDraft
import           Generator.Template              (TemplateDir)
import           StrongPath                      (Abs, Dir, Path)

generateApp :: Path Abs (Dir TemplateDir) -> Gasp -> IO [FileDraft]
generateApp tmplPath gasp = do
  tmplFiles <- readTemplateFiles tmplPath
  return $ map (\v -> makeSimpleTemplateFD v tmplPath gasp) tmplFiles
    ++ generateExternalCodeDir gasp
