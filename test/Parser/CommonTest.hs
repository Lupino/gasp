module Parser.CommonTest where

import           Test.Tasty.Hspec

import           Data.Either
import           Text.Parsec

import           Lexer
import           Parser.Common


spec_parseGaspCommon :: Spec
spec_parseGaspCommon = do

    describe "Parsing gasp element name and properties" $ do
        let parseGaspElementNameAndClosureContent elemKeyword p input =
                runGaspParser (gaspElementNameAndClosureContent elemKeyword p) input

        it "When given valid gasp element declaration along with whitespace parser,\
            \ returns an expected result" $ do
            parseGaspElementNameAndClosureContent "app" whiteSpace "app someApp { }"
                `shouldBe` Right ("someApp", ())

        it "When given valid gasp element declaration along with char parser, returns\
            \ an expected result" $ do
            parseGaspElementNameAndClosureContent "app" (char 'a') "app someApp {a}"
                `shouldBe` Right ("someApp", 'a')

        it "When given gasp element declaration with invalid name, returns Left" $ do
            (isLeft $ parseGaspElementNameAndClosureContent "app" whiteSpace "app 1someApp { }")
                `shouldBe` True

    describe "Parsing gasp closure" $ do
        it "Parses a closure with braces {}" $ do
            runGaspParser (gaspClosure (symbol "content"))  "{ content }"
                `shouldBe` Right "content"

        it "Does not parse a closure with brackets []" $ do
            (isLeft $ runGaspParser (gaspClosure (symbol "content")) "[ content ]")
                `shouldBe` True


    describe "Parsing gasp property - string literal" $ do
        let parseGaspPropertyStringLiteral key input =
                runGaspParser (gaspPropertyStringLiteral key) input

        it "When given key/value with int value, returns Left." $ do
            isLeft (parseGaspPropertyStringLiteral "title" "title: 23")
                `shouldBe` True

        it "When given key/value with string value, returns a parsed value." $ do
            let appTitle = "my first app"
            parseGaspPropertyStringLiteral "title" ("title: \"" ++ appTitle ++ "\"")
                `shouldBe` Right appTitle
