module Command.CreateNewProject
    ( createNewProject
    ) where

import           Command                (Command)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory       (createDirectory, getCurrentDirectory)
import           System.FilePath        ((</>))
import qualified Util.Terminal          as Term


createNewProject :: String -> Command ()
createNewProject projectName = do
    gaspProjectDir <- liftIO $ (</> projectName) <$> getCurrentDirectory
    liftIO $ do
        createDirectory gaspProjectDir
        writeFile (gaspProjectDir </> mainGaspFileInGaspProjectDir) mainGaspFileContent
        writeFile (gaspProjectDir </> gitignoreFileInGaspProjectDir) gitignoreFileContent

    liftIO $ do
        putStrLn $ Term.applyStyles [Term.Green] $ "Created new Gasp project in ./" ++ projectName ++ " directory!"
        putStrLn $ "Move into created directory and type '"
            ++ Term.applyStyles [Term.Bold] "gasp compile"
            ++ "' to compile the app."

  where
      mainGaspFileContent = unlines
          [ "gpio blink LED_BUILTIN"
          , "every toggle_gpio_blink 1000"
          ]

      gitignoreFileContent = unlines
          [ "/build/"
          ]

      mainGaspFileInGaspProjectDir = "main.gasp"
      gitignoreFileInGaspProjectDir = ".gitignore"
