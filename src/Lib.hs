module Lib
    ( compile
    , ProjectRootDir
    , DataDir
    ) where

import qualified Generator
import           Generator.Common    (ProjectRootDir)
import           Generator.Templates (DataDir)
import qualified Parser
import           StrongPath          (Abs, Dir, File, Path)
import qualified StrongPath          as SP


type CompileError = String

compile :: Path Abs File -> Path Abs (Dir ProjectRootDir) -> Path Abs (Dir DataDir) -> IO (Either CompileError ())
compile gaspFile outDir dataDir = do
    gaspStr <- readFile (SP.toFilePath gaspFile)

    case Parser.parseGasp gaspStr of
        Left err   -> return $ Left (show err)
        Right gasp -> generateCode gasp
  where
    generateCode gasp = Generator.writeAppCode gasp outDir dataDir >> return (Right ())
