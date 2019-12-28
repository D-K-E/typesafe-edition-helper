{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model type instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module PrimitiveTest.Pure.ModelType where

-- start def
import           Primitive.Definition.ModelType ( ModelType(..) )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType(..) )
-- end def
-- start fn

import           FunctionDef.Setter        ( StringLike2Primitive(..)
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Transformer   ( Model2StringText(..)
                                                , Model2IdTuple(..)
                                                )

-- end fn

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )

import           Test.Hspec

mtype = StringTypeCons "edition"


main :: IO ()
main = hspec $ do
    describe "ModelType setter test" $ do
        it "Set model type from string" $ fromString "edition" `shouldBe` mtype
        it "Set Model type from Text"
            $          fromText (pack "edition")
            `shouldBe` mtype

        it "Set Model Type from TupleString2Primitive fromTupleString"
            $          fromTupleString ("type", "edition")
            `shouldBe` mtype

        it "Set Model Type from TupleString2Primitive fromTupleText"
            $          fromTupleText ("type", pack "edition")
            `shouldBe` mtype

    describe "ModelType transformer test" $ do
        it "Model2StringText transform model type to string"
            $          toString mtype
            `shouldBe` "edition"

        it "Model2StringText transform model type to Text"
            $          toText mtype
            `shouldBe` pack "edition"

        it "Model2IdTuple transform model type to string ModelId tuple"
            $          toIdTuple mtype
            `shouldBe` ("type", mtype)
