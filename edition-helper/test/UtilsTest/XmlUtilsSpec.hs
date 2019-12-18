{-|
Module : Model
License : see LICENSE
Description : Test suit for xml utilities
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module UtilsTest.XmlUtilsSpec where

import           Utils.XmlUtils                 ( makeName
                                                , makeTagName
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , empty
                                                , unpack
                                                )
import           Text.XML                       ( Name(..) )
import           Test.Hspec
main :: IO ()
main = hspec $ do
    describe "makeName from text" $ do
        it "text to Name type text empty" $ makeName (pack "") `shouldBe` Name
            { nameLocalName = pack "name"
            , nameNamespace = Nothing
            , namePrefix    = Nothing
            }
        it "text to Name type text non empty"
            $          makeName (pack "myname")
            `shouldBe` Name { nameLocalName = pack "myname"
                            , nameNamespace = Nothing
                            , namePrefix    = Nothing
                            }
    describe "makeTagName test" $ do
        it "make tag with empty tag name"
            $          makeTagName (pack "") (pack "edition")
            `shouldBe` makeName (pack "edition")

        it "make tag with non empty tag name"
            $          makeTagName (pack "editag") (pack "edition")
            `shouldBe` makeName (pack "editag")
