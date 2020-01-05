{-|
Module : NodeId
License : see LICENSE
Description : NodeAttr primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.NodeAttr
    ( NodeAttr
    )
where

-- start def
import           Primitive.Definition.Node      ( NodeAttr(TextAttrCons) )
import           Primitive.Definition.Error     ( TextValueError
                                                , IdTupleValueError
                                                , MapValueError(..)
                                                )
-- end def

-- start fn
import           FunctionDef.Setter             ( Map2Primitive(fromStringMap)
                                                , TupleMap2Primitive
                                                    ( fromTupleStringMap
                                                    )
                                                )
import           FunctionDef.Transformer        ( Model2IdTuple(toIdTuple)
                                                , Model2Map
                                                    ( toStringMap
                                                    , toTextMap
                                                    )
                                                )
-- end fn

-- start utility
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text )
import           Utils.MapUtils                 ( convertTxtMap2String
                                                , convertStringMap2Txt
                                                )
-- end utility

-- setter

instance Map2Primitive NodeAttr where
    fromTextMap amap = Right (TextAttrCons amap)

-- end setter
-- transformer

instance Model2Map NodeAttr where
    toTextMap (TextAttrCons   amap) = amap

-- end transformer
