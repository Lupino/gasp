module Main where

import           Data.Version             (showVersion)
import           Paths_gasp               (version)
import           System.Environment       (getArgs)

import           Command                  (runCommand)
import           Command.Clean            (clean)
import           Command.Compile          (compile)
import           Command.CreateNewProject (createNewProject)
import           Command.Watch            (compileAndWatch)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["new", projectName] -> runCommand $ createNewProject projectName
        ["clean"]            -> runCommand clean
        ("compile":xs)       -> runCommand $ compile False xs
        ("syntax":xs)        -> runCommand $ compile True xs
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
    , "  syntax [--low-memory] [--start-addr]"
    , "  compile [--low-memory] [--start-addr] [--template PATH]"
    , "  watch [--low-memory] [--start-addr] [--template PATH]"
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
