{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generator.MockWriteableMonad
       ( MockWriteableMonad
       , MockWriteableMonadLogs(..)
       , MockWriteableMonadConfig(..)
       , getMockLogs
       , defaultMockConfig
       ) where

import           Control.Monad.State
import qualified Data.Aeson                         as Aeson
import           Data.Text                          (Text, pack)
import qualified Path                               as P

import           Fixtures                           (systemPathRoot)
import           Generator.FileDraft.WriteableMonad
import           Generator.Templates                (DataDir, TemplatesDir)
import           StrongPath                         (Abs, Dir, File, Path, Rel)
import qualified StrongPath                         as SP


-- TODO: Instead of manually defining mock like this, consider using monad-mock package,
--   it should do most of this automatically, now there is a lot of boilerplate.
--   Or we ourselves can maybe use template haskell to reduce duplication.

defaultMockConfig :: MockWriteableMonadConfig
defaultMockConfig = MockWriteableMonadConfig
    {
     compileAndRenderTemplate_impl = \_ _ _ -> (pack "Mock template content")
    }

getMockLogs :: MockWriteableMonad a -> MockWriteableMonadConfig -> MockWriteableMonadLogs
getMockLogs mock config = fst $ execState (unMockWriteableMonad mock) (emptyLogs, config)
  where
    emptyLogs = MockWriteableMonadLogs [] [] [] []

instance WriteableMonad MockWriteableMonad where
    writeFileFromText dstPath text = MockWriteableMonad $ do
        modifyLogs (writeFileFromText_addCall dstPath text)

    createDirectoryIfMissing createParents path = MockWriteableMonad $ do
        modifyLogs (createDirectoryIfMissing_addCall createParents path)

    copyFile srcPath dstPath = MockWriteableMonad $ do
        modifyLogs (copyFile_addCall srcPath dstPath)

    compileAndRenderTemplate dataPath tmplPath json = MockWriteableMonad $ do
        modifyLogs (compileAndRenderTemplate_addCall dataPath tmplPath json)
        (_, config) <- get
        return $ (compileAndRenderTemplate_impl config) dataPath tmplPath json

modifyLogs :: MonadState (a, b) m => (a -> a) -> m ()
modifyLogs f = modify (\(logs, config) -> (f logs, config))

newtype MockWriteableMonad a = MockWriteableMonad
    { unMockWriteableMonad :: State (MockWriteableMonadLogs, MockWriteableMonadConfig) a
    }
    deriving (Monad, Applicative, Functor)

data MockWriteableMonadLogs = MockWriteableMonadLogs
    { writeFileFromText_calls :: [(FilePath, Text)]
    , createDirectoryIfMissing_calls :: [(Bool, FilePath)]
    , copyFile_calls :: [(FilePath, FilePath)]
    , compileAndRenderTemplate_calls :: [(Path Abs (Dir DataDir), Path (Rel TemplatesDir) File, Aeson.Value)]
    }

data MockWriteableMonadConfig = MockWriteableMonadConfig
    { compileAndRenderTemplate_impl :: Path Abs (Dir DataDir) -> Path (Rel TemplatesDir) File -> Aeson.Value -> Text
    }

writeFileFromText_addCall :: FilePath -> Text -> MockWriteableMonadLogs -> MockWriteableMonadLogs
writeFileFromText_addCall path text logs =
    logs { writeFileFromText_calls = (path, text):(writeFileFromText_calls logs) }

copyFile_addCall :: FilePath -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
copyFile_addCall srcPath dstPath logs =
    logs { copyFile_calls = (srcPath, dstPath):(copyFile_calls logs) }

createDirectoryIfMissing_addCall :: Bool -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
createDirectoryIfMissing_addCall createParents path logs =
    logs { createDirectoryIfMissing_calls =
           (createParents, path):(createDirectoryIfMissing_calls logs) }

compileAndRenderTemplate_addCall :: Path Abs (Dir DataDir)
                                 -> Path (Rel TemplatesDir) File
                                 -> Aeson.Value
                                 -> MockWriteableMonadLogs
                                 -> MockWriteableMonadLogs
compileAndRenderTemplate_addCall dataPath tmplPath json logs =
    logs { compileAndRenderTemplate_calls =
           (dataPath, tmplPath, json):(compileAndRenderTemplate_calls logs) }
