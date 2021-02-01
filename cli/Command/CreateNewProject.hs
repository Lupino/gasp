module Command.CreateNewProject
    ( createNewProject
    ) where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Path                   as P
import           System.Directory       (createDirectory, getCurrentDirectory)
import qualified System.FilePath        as FP
import           Text.Printf            (printf)

import           Command                (Command, CommandError (..))
import qualified Common
import           StrongPath             (File, Path, Rel, (</>))
import qualified StrongPath             as SP
import qualified Util.Terminal          as Term


createNewProject :: String -> Command ()
createNewProject projectName = do
    absCwd <- liftIO getCurrentDirectory
    gaspProjectDir <- case SP.parseAbsDir $ absCwd FP.</> projectName of
        Left err -> throwError $ CommandError ("Failed to parse absolute path to gasp project dir: " ++ show err)
        Right sp -> return sp
    liftIO $ do
        createDirectorySP gaspProjectDir
        writeFileSP (gaspProjectDir </> mainGaspFileInGaspProjectDir) mainGaspFileContent
        writeFileSP (gaspProjectDir </> gitignoreFileInGaspProjectDir) gitignoreFileContent
        writeFileSP (gaspProjectDir </> Common.buildGaspRootFileInGaspProjectDir)
            "File marking the root of Gasp project."

    liftIO $ do
        putStrLn $ Term.applyStyles [Term.Green] $ "Created new Gasp project in ./" ++ projectName ++ " directory!"
        putStrLn $ "Move into created directory and type '"
            ++ (Term.applyStyles [Term.Bold] "gasp compile")
            ++ "' to compile the app."

  where
      mainGaspFileInGaspProjectDir :: Path (Rel Common.GaspProjectDir) File
      mainGaspFileInGaspProjectDir = SP.fromPathRelFile [P.relfile|main.gasp|]
      mainGaspFileContent = unlines
          [ "app %s {" `printf` projectName
          , "  key: \"%s\"," `printf` projectName
          , "  token: \"%s\"" `printf` projectName
          , "}"
          , ""
          ]

      gitignoreFileInGaspProjectDir :: Path (Rel Common.GaspProjectDir) File
      gitignoreFileInGaspProjectDir = SP.fromPathRelFile [P.relfile|.gitignore|]
      gitignoreFileContent = unlines
          [ "/.gasp/"
          ]

      writeFileSP = writeFile . SP.toFilePath
      createDirectorySP = createDirectory . SP.toFilePath
