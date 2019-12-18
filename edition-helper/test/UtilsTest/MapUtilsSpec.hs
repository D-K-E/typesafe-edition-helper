{-|
Module : Model
License : see LICENSE
Description : Test suit for map utilities
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module UtilsTest.MapUtilsSpec where

import           Utils.MapUtils                 ( add2Map
                                                , convertTxt2NameMap
                                                , convertStringKey
                                                , convertStringVal
                                                , convertTxtKey
                                                , convertTxtVal
                                                , convertStringMap2Txt
                                                , convertTxtMap2String
                                                )
import           Data.Map.Strict                ( Map
                                                , fromList
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Text.XML                       ( Name )
import           Utils.XmlUtils                 ( makeName )

import           Test.Hspec
main :: IO ()
main = hspec $ do
    describe "add key value of text type to Map Text Text add2Map" $ do
        it "should add (Foo, Bar) to map {'my': 'val', 'toto': 'titi'}"
            $          add2Map
                           (fromList
                               [(pack "my", pack "val"), (pack "toto", pack "titi")]
                           )
                           [(pack "Foo", pack "Bar"), (pack "Foo2", pack "Bar2")]
            `shouldBe` fromList
                           [ (pack "my"  , pack "val")
                           , (pack "toto", pack "titi")
                           , (pack "Foo" , pack "Bar")
                           , (pack "Foo2", pack "Bar2")
                           ]

    describe "convert key and value to different types" $ do
        it "convertTxt2NameMap change map Text key to Name key"
            $          convertTxt2NameMap (fromList [(pack "my", pack "val")])
            `shouldBe` (fromList [(makeName (pack "my"), pack "val")])

        it "convertStringKey change map String key to Text key"
            $          convertStringKey (fromList [("my", pack "val")])
            `shouldBe` (fromList [(pack "my", pack "val")])

        it "convertStringVal change map String val to Text val"
            $          convertStringVal (fromList [(pack "my", "val")])
            `shouldBe` (fromList [(pack "my", pack "val")])

        it "convertTxtKey change map Text key to String key"
            $          convertTxtKey (fromList [(pack "my", "val")])
            `shouldBe` (fromList [("my", "val")])

        it "convertTxtVal change map Text val to String val"
            $          convertTxtVal (fromList [("my", pack "val")])
            `shouldBe` (fromList [("my", "val")])

        it "convertStringMap2Txt change map String String to Text Text"
            $          convertStringMap2Txt (fromList [("my", "val")])
            `shouldBe` (fromList [(pack "my", pack "val")])

        it "convertTxtMap2String change map Text Text to String String"
            $          convertTxtMap2String (fromList [(pack "my", pack "val")])
            `shouldBe` (fromList [("my", "val")])
