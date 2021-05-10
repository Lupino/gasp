module Command.CreateNewProject
    ( createNewProject
    ) where

import           Command                (Command, CommandError (..))
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Path                   (File, Path, Rel, parseAbsDir, relfile,
                                         toFilePath, (</>))
import           System.Directory       (createDirectory, getCurrentDirectory)
import qualified System.FilePath        as FP
import qualified Util.Terminal          as Term


createNewProject :: String -> Command ()
createNewProject projectName = do
    absCwd <- liftIO getCurrentDirectory
    gaspProjectDir <- case parseAbsDir $ absCwd FP.</> projectName of
        Left err -> throwError $ CommandError ("Failed to parse absolute path to gasp project dir: " ++ show err)
        Right sp -> return sp
    liftIO $ do
        createDirectorySP gaspProjectDir
        writeFileSP (gaspProjectDir </> mainGaspFileInGaspProjectDir) mainGaspFileContent
        writeFileSP (gaspProjectDir </> gitignoreFileInGaspProjectDir) gitignoreFileContent

    liftIO $ do
        putStrLn $ Term.applyStyles [Term.Green] $ "Created new Gasp project in ./" ++ projectName ++ " directory!"
        putStrLn $ "Move into created directory and type '"
            ++ Term.applyStyles [Term.Bold] "gasp compile"
            ++ "' to compile the app."

  where
      mainGaspFileInGaspProjectDir :: Path Rel File
      mainGaspFileInGaspProjectDir = [relfile|main.gasp|]
      mainGaspFileContent = unlines
          [ "gpio blink LED_BUILTIN"
          , "every toggle_gpio_blink 1000"
          ]

      gitignoreFileInGaspProjectDir :: Path Rel File
      gitignoreFileInGaspProjectDir = [relfile|.gitignore|]
      gitignoreFileContent = unlines
          [ "/build/"
          ]

      writeFileSP = writeFile . toFilePath
      createDirectorySP = createDirectory . toFilePath
