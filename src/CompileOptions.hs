module CompileOptions
    ( CompileOptions(..)
    , CompileType (..)
    , isCompile
    ) where

import           Gasp.Block (Flag)

data CompileType = Syntax | Compile | Eeprom

isCompile :: CompileType -> Bool
isCompile Compile = True
isCompile _       = False


-- TODO(martin): Should these be merged with Gasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
    { externalCodeDirPath :: !FilePath
    , compileType         :: !CompileType
    , projectRootDir      :: !FilePath
    , templateDir         :: !FilePath
    , isProd              :: !Bool
    , argvFlags           :: ![Flag]
    }
