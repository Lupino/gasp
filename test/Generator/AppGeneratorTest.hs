module Generator.AppGeneratorTest where

import           Fixtures                              (systemPathRoot)
import           Gasp
import           Gasp.App
import           Generator.AppGenerator
import qualified Generator.AppGenerator.Common         as Common
import           Generator.FileDraft
import qualified Generator.FileDraft.CopyFileDraft     as CopyFD
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Path                                  as P
import qualified StrongPath                            as SP
import           System.FilePath                       ((</>))
import           Test.Tasty.Hspec

-- TODO(martin): We could define Arbitrary instance for Gasp, define properties over
--   generator functions and then do property testing on them, that would be cool.

spec_AppGenerator :: Spec
spec_AppGenerator = do
    let testApp = (App "TestApp" "TestKey" "TestToken")
    let testGasp = fromGaspElems [GaspElementApp testApp]
        dataDir = SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|a/b|]

    describe "generateApp" $ do
        -- NOTE: This test does not (for now) check that content of files is correct or
        --   that they will successfully be written, it checks only that their
        --   destinations are correct.
        it "Given a simple Gasp, creates file drafts at expected destinations" $ do
            let fileDrafts = generateApp dataDir testGasp
            let expectedFileDraftDstPaths = map ((SP.toFilePath Common.appRootDirInProjectRootDir) </>) $
                    [ "doc.md"
                    , "app.ino"
                    ]

            mapM_
                -- NOTE(martin): I added fd to the pair here in order to have it
                --   printed when shouldBe fails, otherwise I could not know which
                --   file draft failed.
                (\dstPath -> (dstPath, existsFdWithDst fileDrafts dstPath)
                    `shouldBe` (dstPath, True))
                expectedFileDraftDstPaths


existsFdWithDst :: [FileDraft] -> FilePath -> Bool
existsFdWithDst fds dstPath = any ((== dstPath) . getFileDraftDstPath) fds

-- TODO(martin): This should really become part of the Writeable typeclass,
--   since it is smth we want to do for all file drafts.
getFileDraftDstPath :: FileDraft -> FilePath
getFileDraftDstPath (FileDraftTemplateFd fd) = SP.toFilePath $ TmplFD._dstPath fd
getFileDraftDstPath (FileDraftCopyFd fd) = SP.toFilePath $ CopyFD._dstPath fd
