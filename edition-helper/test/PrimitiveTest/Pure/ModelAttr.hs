{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model attribute pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module PrimitiveTest.Pure.ModelAttr where

-- start def
import           Primitive.Definition.ModelAttr ( ModelAttr(..) )
import           Primitive.Instance.ModelAttr
                                                ( ModelAttr(..) )
-- end def
-- start fn
import           FunctionDef.Setter        ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                )
import           FunctionDef.Transformer   ( Model2Map(..)
                                                , Model2IdTuple(..)
                                                )
-- end fn


import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Map.Strict                ( Map
                                                , fromList
                                                )

import           Test.Hspec

mattr = StringAttrCons (fromList [("my", "val")])

main :: IO ()
main = hspec $ do
    describe "Map2Primitive tests" $ do
        it "fromStringMap test"
            $          fromStringMap (fromList [("my", "val")])
            `shouldBe` mattr

        it "fromTextMap test"
            $          fromTextMap (fromList [(pack "my", pack "val")])
            `shouldBe` mattr
        it "fromMixedStrMap test"
            $          fromMixedStrMap (fromList [("my", pack "val")])
            `shouldBe` mattr
        it "fromMixedTextMap test"
            $          fromMixedTextMap (fromList [(pack "my", "val")])
            `shouldBe` mattr

    describe "TupleMap2Primitive tests" $ do
        it "fromTupleStringMap test"
            $          fromTupleStringMap ("mykey", (fromList [("my", "val")]))
            `shouldBe` mattr

        it "fromTupleTextMap test"
            $ fromTupleTextMap ("mykey", (fromList [(pack "my", pack "val")]))
            `shouldBe` mattr

        it "fromTupleMixedStrMap test"
            $ fromTupleMixedStrMap ("mykey", (fromList [("my", pack "val")]))
            `shouldBe` mattr

        it "fromTupleMixedTextMap test"
            $ fromTupleMixedTextMap ("mykey", (fromList [(pack "my", "val")]))
            `shouldBe` mattr

    describe "Model2IdTuple tests" $ do
        it "toIdTuple test" $ toIdTuple mattr `shouldBe` ("attribute", mattr)

    describe "Model2Map tests" $ do
        it "toStringMap test with StringAttrCons"
            $          toStringMap mattr
            `shouldBe` fromList [("my", "val")]

        it "toStringMap test TextAttrCons"
            $ toStringMap (TextAttrCons (fromList [(pack "my", pack "val")]))
            `shouldBe` fromList [("my", "val")]

        it "toTextMap test TextAttrCons"
            $ toTextMap (TextAttrCons (fromList [(pack "my", pack "val")]))
            `shouldBe` fromList [(pack "my", pack "val")]

        it "toTextMap test StringAttrCons" $ toTextMap mattr `shouldBe` fromList
            [(pack "my", pack "val")]
