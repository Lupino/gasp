module Generator
  ( writeAppCode
  ) where

import           Gasp                   (Gasp)
import           Generator.AppGenerator (generateApp)
import           Generator.Common       (ProjectRootDir)
import           Generator.FileDraft    (FileDraft, write)
import           Generator.Templates    (DataDir)
import           StrongPath             (Abs, Dir, Path)

writeAppCode :: Gasp -> Path Abs (Dir ProjectRootDir) -> Path Abs (Dir DataDir) -> IO ()
writeAppCode gasp dstDir dataDir = writeFileDrafts dstDir (generateApp dataDir gasp)

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: Path Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)
