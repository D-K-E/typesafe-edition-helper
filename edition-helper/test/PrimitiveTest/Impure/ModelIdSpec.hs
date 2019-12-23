{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model id impure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.Impure.ModelIdSpec where

-- start def
import           Primitive.Definition.ModelId   ( ModelId(..) )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
import           Primitive.Instance.Impure.ModelId
                                                ( ModelId(..) )
import           Primitive.Definition.Error     ( StringValueError(..) )
-- end def
-- start fn

import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM(..)
                                                , TupleString2PrimitiveM(..)
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM(..)
                                                , Model2IdTupleM(..)
                                                )
import           Control.Impure.ModelId         ( makeModelIdM )
-- end fn
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Control.Monad                  ( Monad )

import           Test.Hspec

str = "smallId12"
mid = StringIdCons str
path1 = "./test.txt"
path2 = "./test2.txt"

main :: IO ()
main = hspec $ do
    describe "ModelId setter test" $ do
        it "set model id from a non empty string" $ case makeModelIdM str of
            Right res -> res `shouldBe` mid

        it "try setting model id with an empty string" $ case makeModelIdM "" of
            Left res -> show res `shouldBe` show (EmptyStr "ModelId")
