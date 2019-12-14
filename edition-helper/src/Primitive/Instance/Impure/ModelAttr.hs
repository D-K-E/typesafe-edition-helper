{-|
Module : Model
License : see LICENSE
Description : ModelAttr primitive instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelAttr
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

import           FunctionDef.Setter             ( ModelAttrSetter(..) )
import           Primitive.Definition.ModelAttr ( ModelAttr
                                                    ( StringAttrCons
                                                    , TextAttrCons
                                                    )
                                                )
import           View.Transformer               ( Model2Map
                                                    ( toTextMap
                                                    , toStringMap
                                                    )
                                                )


instance ModelAttrSetter ModelAttr where
    fromStringMap aMap
        | all null (elems aMap)
        = fail "Attributes must have non empty values"
        | not (all isAlphaNumStr (elems aMap))
        = fail "Attributes must have alphanumeric values"
        | not (all isAsciiStr (elems aMap))
        = fail "Attributes must have ascii values"
        | not (all isAlphaNumStr (keys aMap))
        = fail "Attributes must have alphanumeric keys"
        | not (all isAsciiStr (keys aMap))
        = fail "Attributes must have ascii keys"
        | all null (keys aMap)
        = fail "Attributes must have non empty keys"
        | otherwise
        = return StringAttrCons aMap

instance Model2Map ModelAttr where
    toTextMap (TextAttrCons   aModel) = return aModel
    toTextMap (StringAttrCons aModel) = return convertStringMap2Txt aModel
    toStringMap (StringAttrCons aModel) = return aModel
    toStringMap (TextAttrCons   aModel) = return convertTxtMap2String aModel
