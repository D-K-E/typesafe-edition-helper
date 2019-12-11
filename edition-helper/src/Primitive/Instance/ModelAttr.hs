{-|
Module : Model
License : see LICENSE
Description : ModelAttr primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.ModelAttr
    ( ModelAttr(..)
    )
where

import           Data.Map.Strict                ( Map
                                                , elems
                                                , keys
                                                ) -- importing type
import           Data.Text                      ( Text )
import           Utils.StrUtils                 ( toLowerStr
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )

import           PrimitiveFn.Setter             ( StringLikeSetter(..)
                                                , ModelAttrSetter(..)
                                                )
import           View.Transformer               ( Model2Map(..) )


-- | model attribute: unique key value non nested pairs
data ModelAttr = TextAttrCons (Map Text Text)
                | StringAttrCons (Map String String)
                deriving (Eq, Show)

instance ModelAttrSetter ModelAttr where
    fromStringMap aMap
        | all null (elems aMap)
        = error "Attributes must have non empty values"
        | not (all isAlphaNumStr (elems aMap))
        = error "Attributes must have alphanumeric values"
        | not (all isAsciiStr (elems aMap))
        = error "Attributes must have ascii values"
        | not (all isAlphaNumStr (keys aMap))
        = error "Attributes must have alphanumeric keys"
        | not (all isAsciiStr (keys aMap))
        = error "Attributes must have ascii keys"
        | all null (keys aMap)
        = error "Attributes must have non empty keys"
        | otherwise
        = StringAttrCons aMap

instance Model2Map ModelAttr where
    toTextMap (TextAttrCons   aModel) = aModel
    toTextMap (StringAttrCons aModel) = convertStringMap2Txt aModel
    toStringMap (StringAttrCons aModel) = aModel
    toStringMap (TextAttrCons   aModel) = convertTxtMap2String aModel
