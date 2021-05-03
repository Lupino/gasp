module Generator
  ( writeAppCode
  ) where

import           Gasp                   (Gasp, getGaspExprs, setGaspExprs)
import           Generator.AppGenerator (generateApp, makeSimpleTemplateFD)
import           Generator.FileDraft    (FileDraft, write)
import           Parser                 (parseGasp)
import           Path                   (Abs, Dir, File, Path, Rel, relfile,
                                         (</>))

writeAppCode :: Gasp -> Path Abs Dir -> Path Abs Dir -> IO ()
writeAppCode gasp dstDir tmplDir = do
  writeFileDrafts dstDir [makeSimpleTemplateFD [relfile|combined.gasp|] tmplDir gasp ]
  r <- parseGasp [combinedPath]
  case r of
    Left err       -> error (show err)
    Right combined -> do
      files <- generateApp tmplDir $ setGaspExprs gasp (getGaspExprs gasp ++ getGaspExprs combined)
      writeFileDrafts dstDir files

  where combinedPath = dstDir </> [relfile|combined.gasp|]

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: Path Abs Dir -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)
