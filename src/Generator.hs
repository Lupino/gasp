module Generator
  ( writeAppCode
  ) where

import           Gasp                   (Gasp)
import           Generator.AppGenerator (generateApp)
import           Generator.Common       (ProjectRootDir)
import           Generator.FileDraft    (FileDraft, write)
import           Generator.Templates    (TemplatesDir)
import           StrongPath             (Abs, Dir, Path)

writeAppCode :: Gasp -> Path Abs (Dir ProjectRootDir) -> Path Abs (Dir TemplatesDir) -> IO ()
writeAppCode gasp dstDir tmplDir = do
  files <- generateApp tmplDir gasp
  writeFileDrafts dstDir files

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: Path Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)
