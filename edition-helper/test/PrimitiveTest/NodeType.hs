{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive node type instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.NodeType where

-- start def
import           Primitive.Definition.Node      ( NodeType(..) )
import           Primitive.Instance.NodeType
-- end def

-- start fn

import           Control.NodeType               ( makeNodeTypeFromIdTuple
                                                , makeNodeTypeFromText
                                                )
import           FunctionDef.Transformer        ( Model2IdTuple(..)
                                                , NodeIdType2Text(..)
                                                )

-- end fn
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )



import           Test.Hspec

mtype = TextTypeCons "edition"

main :: IO ()
main = hspec $ do
    describe "NodeType setter test" $ do
        it "Set Model type from Text"
            $          makeNodeTypeFromText (pack "edition")
            `shouldBe` Right mtype

        it "Check for invalid text value"
            $          makeNodeTypeFromText (pack "someval")
            `shouldBe` Left
                           (OtherTextError
                               (pack ("Unsupported type: " ++ "someval"))
                           )

        it "Set Model Type from IdTuple2Node"
            $          makeNodeTypeFromIdTuple (pack "type", pack "edition")
            `shouldBe` Right mtype

        it "Check for bad id value"
            $          makeNodeTypeFromIdTuple (pack "tyype", pack "edition")
            `shouldBe` Left
                           (OtherTextError
                               pack
                               ("IdTuple first argument has inappropriate value: "
                               ++ "tyype"
                               )
                           )


    describe "NodeType transformer test" $ do

        it "NodeIdType2Text transform model type to Text"
            $          toText mtype
            `shouldBe` pack "edition"

        it "Model2IdTuple transform model type to string NodeId tuple"
            $          toIdTuple mtype
            `shouldBe` (pack "type", mtype)
