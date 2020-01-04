{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive model info pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.Pure.NodeInfo where

-- start def
import           Primitive.Definition.NodeInfo ( NodeInfo(..) )
import           Primitive.Instance.NodeInfo
                                                ( NodeInfo(..) )
import           Primitive.Definition.NodeId   ( NodeId(..) )
import           Primitive.Definition.NodeAttr ( NodeAttr(..) )
import           Primitive.Definition.NodeType ( NodeType(..) )
import           Primitive.Instance.NodeId
                                                ( NodeId(..) )
import           Primitive.Instance.NodeAttr
                                                ( NodeAttr(..) )
import           Primitive.Instance.NodeType
                                                ( NodeType(..) )
-- end def
-- start fn
import           FunctionDef.Setter        ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                , InfoTuple2Primitive(..)
                                                )
import           FunctionDef.Transformer   ( Model2Map(..)
                                                , Model2IdTuple(..)
                                                , NodeInfo2Tuple(..)
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

    describe "NodeInfo2Tuple tests" $ do
        it "toInfoTuple test" $ toInfoTuple minfo `shouldBe` tpl
