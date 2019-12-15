{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelId
    ( ModelId
    )
where

-- start def
import           Primitive.Definition.ModelId   ( ModelId
                                                    ( StringIdCons
                                                    , TextIdCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId )
-- end def
-- start functionality
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , empty
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )

import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromString)
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
-- end functionality

instance StringLikeSetterM ModelId where
    fromStringM aStr
        | null aStr
        = fail "empty string is not allowed as id"
        | not (isAlphaNumStr aStr)
        = fail "Only ascii alphanumeric strings are allowed"
        | not (isAsciiStr aStr)
        = fail "Only ascii alphanumeric strings are allowed"

instance Model2StringTextM ModelId where
    toStringM (StringIdCons aModel) | null aModel =
        fail "Null model id to string"
    toStringM (TextIdCons aModel) | empty aModel = fail "Null model id to text"
    toTextM (TextIdCons aModel) | empty aModel  = fail "Null model id to text"
    toTextM (StringIdCons aModel) | null aModel = fail "Null model id to string"
