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

import           FunctionDef.Pure.Setter        ( StringLike2Primitive(..)
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Pure.Transformer   ( Model2StringText(..)
                                                , Model2IdTuple(..)
                                                )

-- end fn

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )

import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "ModelType setter test" $ do
        it "Set model type from string"
            $          fromString "edition"
            `shouldBe` StringTypeCons "edition"

        it "Set Model type from Text"
            $          fromText (pack "edition")
            `shouldBe` StringTypeCons "edition"

        it "Set Model Type from TupleString2Primitive fromTupleString"
            $          fromTupleString ("type", "edition")
            `shouldBe` StringTypeCons "edition"

        it "Set Model Type from TupleString2Primitive fromTupleText"
            $          fromTupleText ("type", pack "glossary")
            `shouldBe` StringTypeCons "glossary"

    describe "ModelType transformer test" $ do
        it "Model2StringText transform model type to string"
            $          toString (StringTypeCons "analysis")
            `shouldBe` "analysis"

        it "Model2StringText transform model type to Text"
            $          toText (StringTypeCons "analysis")
            `shouldBe` pack "analysis"

        it "Model2IdTuple transform model type to string ModelId tuple"
            $          toIdTuple (StringTypeCons "edition")
            `shouldBe` ("type", StringTypeCons "edition")
