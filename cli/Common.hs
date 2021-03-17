module Common
    ( buildGaspDirInGaspProjectDir
    , buildGaspRootFileInGaspProjectDir
    , extCodeDirInGaspProjectDir
    , gaspSays
    ) where

import           Path          (Dir, File, Path, Rel, reldir, relfile)
import qualified Util.Terminal as Term

buildGaspDirInGaspProjectDir :: Path Rel Dir
buildGaspDirInGaspProjectDir = [reldir|build|]

buildGaspRootFileInGaspProjectDir :: Path Rel File
buildGaspRootFileInGaspProjectDir = [relfile|.gasproot|]

gaspSays :: String -> IO ()
gaspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

extCodeDirInGaspProjectDir :: Path Rel Dir
extCodeDirInGaspProjectDir = [reldir|src|]
