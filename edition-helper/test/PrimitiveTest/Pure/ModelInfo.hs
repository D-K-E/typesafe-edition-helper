{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model info pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.Pure.ModelInfo where

-- start def
import           Primitive.Definition.ModelInfo ( ModelInfo(..) )
import           Primitive.Instance.ModelInfo
                                                ( ModelInfo(..) )
import           Primitive.Definition.ModelId   ( ModelId(..) )
import           Primitive.Definition.ModelAttr ( ModelAttr(..) )
import           Primitive.Definition.ModelType ( ModelType(..) )
import           Primitive.Instance.ModelId
                                                ( ModelId(..) )
import           Primitive.Instance.ModelAttr
                                                ( ModelAttr(..) )
import           Primitive.Instance.ModelType
                                                ( ModelType(..) )
-- end def
-- start fn
import           FunctionDef.Setter        ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                , InfoTuple2Primitive(..)
                                                )
import           FunctionDef.Transformer   ( Model2Map(..)
                                                , Model2IdTuple(..)
                                                , ModelInfo2Tuple(..)
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

minfo = InfoCons { modelId   = StringIdCons "my-id"
                 , modelType = StringTypeCons "edition"
                 , modelAttr = StringAttrCons (fromList [("my", "val")])
                 }

tpl =
    ( StringIdCons "my-id"
    , StringTypeCons "edition"
    , StringAttrCons (fromList [("my", "val")])
    )

main :: IO ()
main = hspec $ do
    describe "InfoTuple2Primitive tests" $ do
        it "fromInfoTuple test" $ fromInfoTuple tpl `shouldBe` minfo

    describe "ModelInfo2Tuple tests" $ do
        it "toInfoTuple test" $ toInfoTuple minfo `shouldBe` tpl
