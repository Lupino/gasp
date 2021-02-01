module Common
    ( GaspProjectDir
    , buildGaspDirInGaspProjectDir
    , buildGaspRootFileInGaspProjectDir
    , gaspSays
    ) where

import qualified Path             as P

import qualified Generator.Common
import           StrongPath       (Dir, File, Path, Rel)
import qualified StrongPath       as SP
import qualified Util.Terminal    as Term


data GaspProjectDir -- Root dir of Gasp project, containing source files.

-- TODO: SHould this be renamed to include word "root"?
buildGaspDirInGaspProjectDir :: Path (Rel GaspProjectDir) (Dir Generator.Common.ProjectRootDir)
buildGaspDirInGaspProjectDir = SP.fromPathRelDir [P.reldir|build|]

buildGaspRootFileInGaspProjectDir :: Path (Rel GaspProjectDir) File
buildGaspRootFileInGaspProjectDir = SP.fromPathRelFile [P.relfile|.gasproot|]

gaspSays :: String -> IO ()
gaspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what
