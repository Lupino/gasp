module Generator.ExternalCodeGenerator
       ( generateExternalCodeDir
       ) where

import qualified ExternalCode                           as EC
import           Gasp                                   (Gasp)
import qualified Gasp
import           Generator.Common                       (ProjectRootDir)
import qualified Generator.ExternalCodeGenerator.Common as C
import qualified Generator.FileDraft                    as FD
import           StrongPath                             (Dir, File, Path, Rel,
                                                         (</>))


-- | Takes external code files from Gasp and generates them in new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
generateExternalCodeDir :: Path (Rel ProjectRootDir) (Dir C.GeneratedExternalCodeDir)
                        -> Gasp
                        -> [FD.FileDraft]
generateExternalCodeDir strategy gasp =
    map (generateFile strategy) (Gasp.getExternalCodeFiles gasp)

generateFile :: Path (Rel ProjectRootDir) (Dir C.GeneratedExternalCodeDir) -> EC.File -> FD.FileDraft
generateFile strategy file =
  let relDstPath = strategy </> dstPathInGenExtCodeDir
      absSrcPath = EC.fileAbsPath file
  in FD.createCopyFileDraft relDstPath absSrcPath
  where
    dstPathInGenExtCodeDir :: Path (Rel C.GeneratedExternalCodeDir) File
    dstPathInGenExtCodeDir = C.castRelPathFromSrcToGenExtCodeDir $ EC.filePathInExtCodeDir file
