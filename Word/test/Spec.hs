import Test.Hspec
import Lib
import Data 


main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            formatGrid ["ab", "cd"] `shouldBe` "ab\ncd\n"

    describe "findWord" $ do
        it "Should find word on grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"            
            findWord grid "CSHARP" `shouldBe` Just "CSHARP"
            findWord grid "PERL" `shouldBe` Just "PERL"
        
        it "Should NOT find word that do not exist on grid" $ do
            findWord grid "HAMSTER" `shouldBe` Nothing

        it "Should find all the words exists on grid" $ do
            findWords grid languages `shouldBe` languages

        it "Should NOT find all the words that DOES NOT exists on grid" $ do
            findWords grid ["FRECH", "VIETNAMES"] `shouldBe` []