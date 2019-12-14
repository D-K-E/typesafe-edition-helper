{-|
Module : Model
License : see LICENSE
Description : ModelAttr primitive impure instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelAttr
    ( ModelAttr
    )
where

-- start def
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr )
-- end def

import           Data.Map.Strict                ( elems
                                                , keys
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )

import           FunctionDef.Impure.Setter      ( ModelAttrSetterM(..) )
import           FunctionDef.Impure.Transformer ( Model2MapM
                                                    ( toTextMapM
                                                    , toStringMapM
                                                    )
                                                )


instance ModelAttrSetterM ModelAttr where
    fromStringMapM aMap
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

instance Model2MapM ModelAttr where
    toTextMapM (TextAttrCons aModel) =
        fail "transforming to text map has failed"
    toTextMapM (StringAttrCons aModel) =
        fail "transforming to text map from underlaying string map has failed"
    toStringMapM (StringAttrCons aModel) =
        fail "transforming to string map has failed"
    toStringMapM (TextAttrCons aModel) =
        fail "transforming to string map from underlaying text map has failed"
