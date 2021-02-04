module Generator.ExternalCodeGenerator.Common
    ( GeneratedExternalCodeDir
    , castRelPathFromSrcToGenExtCodeDir
    , asGenExtFile
    ) where

import           ExternalCode (SourceExternalCodeDir)
import qualified Path         as P
import           StrongPath   (File, Path, Rel)
import qualified StrongPath   as SP


data GeneratedExternalCodeDir -- ^ Path to the directory where ext code will be generated.

asGenExtFile :: P.Path P.Rel P.File -> Path (Rel GeneratedExternalCodeDir) File
asGenExtFile = SP.fromPathRelFile

castRelPathFromSrcToGenExtCodeDir :: Path (Rel SourceExternalCodeDir) a -> Path (Rel GeneratedExternalCodeDir) a
castRelPathFromSrcToGenExtCodeDir = SP.castRel
