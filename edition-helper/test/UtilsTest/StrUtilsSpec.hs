{-|
Module : Model
License : see LICENSE
Description : Test suit for string utilities
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module UtilsTest.StrUtilsSpec where
import           Utils.StrUtils                 ( toLowerStr
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                , appendOrPrepend
                                                )
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "to lower string, toLowerStr" $ do
        it "should transform to lower case when arg contains upper case"
            $          toLowerStr "EdiTion"
            `shouldBe` "edition"


        it "should return str as is if arg is lower case"
            $          toLowerStr "edition"
            `shouldBe` "edition"

    describe "check alphanumeric string isAlphaNumStr" $ do
        it "if arg is alphanumeric should return true"
            $          isAlphaNumStr "e13di3500tion"
            `shouldBe` True

        it "if arg is not alphanumeric should return false"
            $          isAlphaNumStr "e;,$13di3500tion"
            `shouldBe` False


    describe "check string isAsciiStr" $ do
        it "if arg is ascii should return true"
            $          isAsciiStr "e13di3500tion"
            `shouldBe` True

        it "if arg is not ascii should return false"
            $          isAsciiStr "eḫ13di3500tion"
            `shouldBe` False

    describe "append or prepend strings" $ do
        it "if bool arg is true it should append"
            $          appendOrPrepend "edition" "note" True
            `shouldBe` "edition-note"

        it "if bool arg is false it should prepend"
            $          appendOrPrepend "edition" "note" False
            `shouldBe` "note-edition"
