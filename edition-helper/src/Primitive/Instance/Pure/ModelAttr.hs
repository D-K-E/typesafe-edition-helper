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

import           Data.Map.Strict                ( elems
                                                , keys
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )

import           FunctionDef.Pure.Setter        ( ModelAttrSetter(..) )
import           Primitive.Definition.ModelAttr ( ModelAttr
                                                    ( StringAttrCons
                                                    , TextAttrCons
                                                    )
                                                )
import           FunctionDef.Pure.Transformer   ( Model2Map
                                                    ( toTextMap
                                                    , toStringMap
                                                    )
                                                )

instance ModelAttrSetter ModelAttr where
    fromStringMap = StringAttrCons

instance Model2Map ModelAttr where
    toTextMap (TextAttrCons   aModel) = aModel
    toTextMap (StringAttrCons aModel) = convertStringMap2Txt aModel
    toStringMap (StringAttrCons aModel) = aModel
    toStringMap (TextAttrCons   aModel) = convertTxtMap2String aModel
