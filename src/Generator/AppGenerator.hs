module Generator.AppGenerator
       ( generateApp
       ) where

import           Gasp
import           Generator.AppGenerator.Common   (asTmplFile,
                                                  extCodeDirInProjectRootDir,
                                                  makeSimpleTemplateFD)
import           Generator.ExternalCodeGenerator (generateExternalCodeDir)
import           Generator.FileDraft
import           Generator.Templates             (DataDir)
import qualified Path                            as P (relfile)
import           StrongPath                      (Abs, Dir, Path)

generateApp :: Path Abs (Dir DataDir) -> Gasp -> [FileDraft]
generateApp dataPath gasp =
  [genApp dataPath gasp, genDocs dataPath gasp]
  ++ generateExternalCodeDir extCodeDirInProjectRootDir gasp

genApp :: Path Abs (Dir DataDir) -> Gasp -> FileDraft
genApp dataPath = makeSimpleTemplateFD (asTmplFile [P.relfile|app.ino|]) dataPath

genDocs :: Path Abs (Dir DataDir) -> Gasp -> FileDraft
genDocs dataPath = makeSimpleTemplateFD (asTmplFile [P.relfile|doc.md|]) dataPath
