module Generator.AppGenerator
       ( generateApp
       ) where

import           Gasp
import           Generator.AppGenerator.Common (asTmplFile,
                                                makeSimpleTemplateFD)
import           Generator.FileDraft
import qualified Path                          as P (relfile)

generateApp :: Gasp -> [FileDraft]
generateApp gasp = [genApp gasp, genDocs gasp]

genApp :: Gasp -> FileDraft
genApp = makeSimpleTemplateFD (asTmplFile [P.relfile|app.ino|])

genDocs :: Gasp -> FileDraft
genDocs = makeSimpleTemplateFD (asTmplFile [P.relfile|doc.md|])
