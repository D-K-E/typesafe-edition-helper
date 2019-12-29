{-|
Module : ModelId
License : see LICENSE
Description : ModelAttr primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelAttr where

-- start def
import           Primitive.Definition.ModelAttr ( ModelAttr
                                                    ( StringAttrCons
                                                    , TextAttrCons
                                                    )
                                                )
import           Primitive.Definition.Error     ( StringValueError
                                                , IdTupleValueError
                                                , MapValueError
                                                )
-- end def

-- start fn
import           FunctionDef.Setter             ( Map2Primitive(fromStringMap)
                                                , TupleMap2Primitive
                                                    ( fromTupleString
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
instance Map2Primitive ModelAttr where
    fromStringMap amap = Right (StringAttrCons amap)


instance TupleMap2Primitive ModelAttr where
    fromTupleStringMap (astr, amap) = Right (StringAttrCons amap)

-- end setter
-- transformer
instance Model2IdTuple ModelAttr where
    toIdTuple mdl = ("attribute", mdl)

instance Model2Map ModelAttr where
    toTextMap (TextAttrCons   amap) = amap
    toTextMap (StringAttrCons amap) = convertStringMap2Txt amap
    toStringMap (StringAttrCons amap) = amap
    toStringMap (TextAttrCons   amap) = convertTxtMap2String amap
-- end transformer
