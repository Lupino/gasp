module Common
  ( buildGaspDirInGaspProjectDir
  , mainGaspFile
  , extCodeDirInGaspProjectDir
  , gaspSays
  ) where

import           Path          (Dir, File, Path, Rel, reldir, relfile)
import qualified Util.Terminal as Term

buildGaspDirInGaspProjectDir :: Path Rel Dir
buildGaspDirInGaspProjectDir = [reldir|build|]

mainGaspFile :: Path Rel File
mainGaspFile = [relfile|main.gasp|]

gaspSays :: String -> IO ()
gaspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

extCodeDirInGaspProjectDir :: Path Rel Dir
extCodeDirInGaspProjectDir = [reldir|src|]
