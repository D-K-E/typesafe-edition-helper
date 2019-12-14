{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelId
    ( ModelId
    )
where

import           Primitive.Definition.ModelId   ( ModelId
                                                    ( StringIdCons
                                                    , TextIdCons
                                                    )
                                                )
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )

import           FunctionDef.Setter             ( StringLikeSetter
                                                , fromString
                                                )
import           View.Transformer               ( Model2StringText
                                                , toString
                                                , toText
                                                )


instance StringLikeSetter ModelId where
    fromString aStr
        | null aStr
        = fail "empty string is not allowed as id"
        | not (isAlphaNumStr aStr)
        = fail "Only ascii alphanumeric strings are allowed"
        | not (isAsciiStr aStr)
        = fail "Only ascii alphanumeric strings are allowed"
        | otherwise
        = return (StringIdCons aStr)

instance Model2StringText ModelId where
    toString (StringIdCons aModel) = aModel
    toString (TextIdCons   aModel) = unpack aModel
    toText (TextIdCons   aModel) = aModel
    toText (StringIdCons aModel) = pack aModel
