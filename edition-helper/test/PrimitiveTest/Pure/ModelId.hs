{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model id pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module PrimitiveTest.Pure.ModelId where

-- start def
import           Primitive.Definition.ModelId   ( ModelId(..) )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
-- end def
-- start fn

import           FunctionDef.Pure.Setter        ( StringLike2Primitive(..)
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Pure.Transformer   ( Model2StringText(..)
                                                , Model2IdTuple(..)
                                                )
import           FunctionDef.Pure.Matcher
import           FunctionDef.Pure.Modifier

-- end fn

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )

import           Test.Hspec

mid = StringIdCons "small-id-12"


main :: IO ()
main = hspec $ do
    describe "ModelId setter test" $ do
        it "Set model id from string" $ fromString "small-id-12" `shouldBe` mid
        it "Set Model id from Text"
            $          fromText (pack "small-id-12")
            `shouldBe` mid

        it "Set Model Id from TupleString2Primitive fromTupleString"
            $          fromTupleString ("id", "small-id-12")
            `shouldBe` mid

        it "Set Model Id from TupleString2Primitive fromTupleText"
            $          fromTupleText ("id", pack "small-id-12")
            `shouldBe` mid

    describe "ModelId transformer test" $ do
        it "Model2StringText transform model id to string"
            $          toString mid
            `shouldBe` "small-id-12"

        it "Model2StringText transform model id to Text"
            $          toText mid
            `shouldBe` pack "small-id-12"

        it "Model2IdTuple transform model id to string ModelId tuple"
            $          toIdTuple mid
            `shouldBe` ("id", mid)
