module Main where

import           Data.Version             (showVersion)
import           Paths_gasp               (version)
import           System.Environment       (getArgs)

import           Command                  (runCommand)
import           Command.Clean            (clean)
import           Command.Compile          (compile)
import           Command.CreateNewProject (createNewProject)
import           Command.Watch            (compileAndWatch)
import           CompileOptions           (CompileType (..))


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["new", projectName] -> runCommand $ createNewProject projectName
        ["clean"]            -> runCommand clean
        ("compile":xs)       -> runCommand $ compile Compile xs
        ("syntax":xs)        -> runCommand $ compile Syntax xs
        ("eeprom":xs)        -> runCommand $ compile Eeprom xs
        ("watch":xs)         -> runCommand $ compileAndWatch xs
        ["version"]          -> printVersion
        _                    -> printUsage

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage:"
    , "  gasp <command> [command-args]"
    , ""
    , "Commands:"
    , "  new <project-name>"
    , "  syntax [--production]"
    , "  eeprom [--production]"
    , "  compile [--template PATH] [--production]"
    , "  watch [--template PATH] [--production]"
    , "  clean"
    , "  version"
    , ""
    , "Examples:"
    , "  gasp new MyApp"
    , ""
    , "Custom Template:"
    , "  export GASP_TEMPLATE_PATH=you/custom/template/path"
    , ""
    ]

printVersion :: IO ()
printVersion = putStrLn $ showVersion version
