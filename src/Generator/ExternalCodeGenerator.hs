module Generator.ExternalCodeGenerator
       ( generateExternalCodeDir
       ) where

import qualified ExternalCode        as EC
import           Gasp                (Gasp)
import qualified Gasp
import           Generator.Common    (ProjectRootDir)
import qualified Generator.FileDraft as FD
import           StrongPath          (File, Path, Rel)
import qualified StrongPath          as SP


-- | Takes external code files from Gasp and generates them in new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
generateExternalCodeDir :: Gasp
                        -> [FD.FileDraft]
generateExternalCodeDir gasp =
    map generateFile (Gasp.getExternalCodeFiles gasp)

generateFile :: EC.File -> FD.FileDraft
generateFile file =
  let relDstPath = dstPathInGenExtCodeDir
      absSrcPath = EC.fileAbsPath file
  in FD.createCopyFileDraft relDstPath absSrcPath
  where
    dstPathInGenExtCodeDir :: Path (Rel ProjectRootDir) File
    dstPathInGenExtCodeDir = SP.castRel $ EC.filePathInExtCodeDir file
