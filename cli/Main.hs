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
        ["compile"]          -> runCommand $ compile False
        ["syntax"]           -> runCommand $ compile True
        ["watch"]            -> runCommand compileAndWatch
        ["version"]          -> printVersion
        _                    -> printUsage

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage:"
    , "  gasp <command> [command-args]"
    , ""
    , "Commands:"
    , "  new <project-name>"
    , "  syntax"
    , "  compile"
    , "  watch"
    , "  clean"
    , "  version"
    , ""
    , "Examples:"
    , "  gasp new MyApp"
    , ""
    ]

printVersion :: IO ()
printVersion = putStrLn $ showVersion version
