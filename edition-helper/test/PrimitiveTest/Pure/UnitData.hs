{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive unit data pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.Pure.UnitData where

-- start def
import           Primitive.Definition.UnitData  ( UnitData(..) )
import           Primitive.Instance.UnitData
                                                ( UnitData(..) )
-- end def

-- start fn
import           FunctionDef.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Transformer   ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
-- end utility
import           Test.Hspec

udata = StringUnitDataCons "mydata"

utpli = ("data-unit", udata)

utpls = ("data-unit", "mydata")
utplt = ("data-unit", pack "mydata")

main :: IO ()
main = hspec $ do
    describe "StringLike2Primitive tests" $ do
        it "fromString test" $ fromString "mydata" `shouldBe` udata
        it "fromText test" $ fromText (pack "mydata") `shouldBe` udata
    describe "TupleString2Primitive tests" $ do
        it "fromTupleString test" $ fromTupleString utpls `shouldBe` udata
        it "fromTupleText test" $ fromTupleText utplt `shouldBe` udata
    describe "Model2StringText tests" $ do
        it "toString test" $ toString udata `shouldBe` "mydata"
        it "toText test" $ toText udata `shouldBe` pack "mydata"
    describe "Model2IdTuple tests" $ do
        it "toIdTuple test" $ toIdTuple udata `shouldBe` utpli
