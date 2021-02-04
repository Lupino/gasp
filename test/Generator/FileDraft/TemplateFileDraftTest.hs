module Generator.FileDraft.TemplateFileDraftTest where

import           Test.Tasty.Hspec

import           Data.Aeson                   (object, (.=))
import           Data.Text                    (Text)
import qualified Path                         as P

import           Generator.FileDraft
import qualified StrongPath                   as SP

import           Fixtures                     (systemPathRoot)
import qualified Generator.MockWriteableMonad as Mock


spec_TemplateFileDraft :: Spec
spec_TemplateFileDraft = do
    describe "write" $ do
        it "Creates new file from existing template file" $ do
            let mock = write dstDir fileDraft
            let mockLogs = Mock.getMockLogs mock mockConfig
            Mock.compileAndRenderTemplate_calls mockLogs
                `shouldBe` [(templatePath, templateData)]
            Mock.createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, SP.toFilePath $ SP.parent expectedDstPath)]
            Mock.writeFileFromText_calls mockLogs
                `shouldBe` [(SP.toFilePath expectedDstPath, mockTemplateContent)]
              where
                (dstDir, dataDir, dstPath, templatePath) =
                    ( SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|a/b|]
                    , SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|a/b|]
                    , SP.fromPathRelFile [P.relfile|c/d/dst.txt|]
                    , SP.fromPathRelFile [P.relfile|e/tmpl.txt|]
                    )
                templateData = object [ "foo" .= ("bar" :: String) ]
                fileDraft = createTemplateFileDraft dstPath dataDir templatePath (Just templateData)
                expectedDstPath = dstDir SP.</> dstPath
                mockTemplateContent = "Mock template content" :: Text
                mockConfig = Mock.defaultMockConfig
                    { Mock.compileAndRenderTemplate_impl = \_ _ -> mockTemplateContent
                    }
