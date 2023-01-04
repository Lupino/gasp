module Generator
  ( writeAppCode
  ) where

import           Gasp                   (Gasp, getGaspExprs, setGaspExprs)
import           Generator.AppGenerator (generateApp)
import           Generator.FileDraft    (FileDraft, write)
import           Parser                 (parseGasp)
import           System.FilePath        ((</>))

stage0 :: FilePath -> Bool
stage0 fn = take 7 fn == "stage0/"

stage1 :: FilePath -> Bool
stage1 fn = take 7 fn == "stage1/"

stage2 :: FilePath -> Bool
stage2 fn = take 7 fn == "stage2/"

writeAppCode :: Gasp -> FilePath -> FilePath -> IO ()
writeAppCode gasp dstDir tmplDir = do
  files0 <- generateApp stage0 tmplDir gasp
  writeFileDrafts dstDir files0

  r0 <- parseGasp "" $ dstDir </> fn0
  case r0 of
    Left err0       -> error (show err0)
    Right main0 -> do
      let exprs0 = getGaspExprs main0
      files1 <- generateApp stage1 tmplDir $ setGaspExprs gasp (exprs ++ exprs0)
      writeFileDrafts dstDir files1

      r1 <- parseGasp "" $ dstDir </> fn1
      case r1 of
        Left err1       -> error (show err1)
        Right main1 -> do
          let exprs1 = getGaspExprs main1
          files2 <- generateApp stage2 tmplDir $ setGaspExprs gasp (exprs ++ exprs0 ++ exprs1)
          writeFileDrafts dstDir files2

  where fn1 = "stage1/main.gasp"
        fn0 = "stage0/main.gasp"
        exprs = getGaspExprs gasp

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: FilePath -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)
