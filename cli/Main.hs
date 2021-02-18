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
        ["new", projectName]        -> runCommand $ createNewProject projectName
        ["clean"]                   -> runCommand clean
        ["compile"]                 -> runCommand $ compile False False
        ["syntax"]                  -> runCommand $ compile True False
        ["compile", "--low-memory"] -> runCommand $ compile False True
        ["syntax", "--low-memory"]  -> runCommand $ compile True True
        ["watch"]                   -> runCommand $ compileAndWatch False
        ["watch", "--low-memory"]   -> runCommand $ compileAndWatch True
        ["version"]                 -> printVersion
        _                           -> printUsage

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage:"
    , "  gasp <command> [command-args]"
    , ""
    , "Commands:"
    , "  new <project-name>"
    , "  syntax [--low-memory]"
    , "  compile [--low-memory]"
    , "  watch [--low-memory]"
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
