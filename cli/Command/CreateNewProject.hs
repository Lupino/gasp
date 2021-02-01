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
          , "init {=code"
          , "#define GL_SERIAL Serial"
          , "#define DEBUG_SERIAL Serial"
          , "#define SEND_DELAY_MS send_delay_ms"
          , "code=}"
          , ""
          , "setup {=code"
          , "    GL_SERIAL.begin(115200);"
          , "    while (!GL_SERIAL) {;}"
          , "code=}"
          , ""
          , "attr delay {"
          , "  var: send_delay_ms,"
          , "  type: \"unsigned long\","
          , "  default: 300,"
          , "  min: 60,"
          , "  max: 86400,"
          , "  scale: 1000"
          , "}"
          , ""
          , "metric temp {"
          , "  var: temperature,"
          , "  type: \"float\","
          , "  max: 100,"
          , "  min: 0,"
          , "  threshold: 1,"
          , "  prec: 2"
          , "}"
          , ""
          , "func read_temp {=code"
          , "    temperature = 25.8;"
          , "    return RET_SUCC;"
          , "code=}"
          , ""
          , "monitor %s {" `printf` projectName
          , "  fn: read_temp,"
          , "  delay_ms: 6000"
          , "}"
          ]

      gitignoreFileInGaspProjectDir :: Path (Rel Common.GaspProjectDir) File
      gitignoreFileInGaspProjectDir = SP.fromPathRelFile [P.relfile|.gitignore|]
      gitignoreFileContent = unlines
          [ "/build/"
          ]

      writeFileSP = writeFile . SP.toFilePath
      createDirectorySP = createDirectory . SP.toFilePath
