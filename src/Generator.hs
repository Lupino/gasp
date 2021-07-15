module Generator
  ( writeAppCode
  ) where

import           Gasp                   (Gasp, getGaspExprs, setGaspExprs)
import           Generator.AppGenerator (generateApp)
import           Generator.FileDraft    (FileDraft, write)
import           Parser                 (parseGasp)
import           System.FilePath        ((</>))

writeAppCode :: Gasp -> FilePath -> FilePath -> IO ()
writeAppCode gasp dstDir tmplDir = do
  files0 <- generateApp tmplDir gasp
  writeFileDrafts dstDir files0

  r <- parseGasp $ dstDir </> fn
  case r of
    Left err       -> error (show err)
    Right combined -> do
      files1 <- generateApp tmplDir $ setGaspExprs gasp (getGaspExprs gasp ++ getGaspExprs combined)
      writeFileDrafts dstDir files1

  where fn = "combined.gasp"

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: FilePath -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)
