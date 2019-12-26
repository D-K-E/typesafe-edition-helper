{-|
Module : Model
License : see LICENSE
Description : ModelAttr primitive pure instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelAttr
    ( ModelAttr
    )
where

-- start def
import           Primitive.Definition.ModelAttr ( ModelAttr
                                                    ( StringAttrCons
                                                    , TextAttrCons
                                                    )
                                                )
-- end def

-- start functionality
import           FunctionDef.Pure.Setter        ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                )

import           FunctionDef.Pure.Transformer   ( Model2Map
                                                    ( toTextMap
                                                    , toStringMap
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )
-- end functionality
-- start utility
import           Data.Map.Strict                ( elems
                                                , keys
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )
-- end utility

-- start setter

instance Map2Primitive ModelAttr where
    fromStringMap amap = Right (StringAttrCons amap)

instance TupleMap2Primitive ModelAttr where
    fromTupleStringMap tpl = Right (StringAttrCons (snd tpl))

-- end setter

instance Model2IdTuple ModelAttr where
    toIdTuple mattr = ("attribute", mattr)

instance Model2Map ModelAttr where
    toTextMap (TextAttrCons   aModel) = aModel
    toTextMap (StringAttrCons aModel) = convertStringMap2Txt aModel
    toStringMap (StringAttrCons aModel) = aModel
    toStringMap (TextAttrCons   aModel) = convertTxtMap2String aModel
