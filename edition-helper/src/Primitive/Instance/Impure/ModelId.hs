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
import           Primitive.Definition.Error     ( StringValueError(..) )
-- end def
-- start fn
import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM
                                                    ( fromStringM
                                                    )
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
-- end fn
-- start utility
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Control.Exception              ( throw )
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , empty
                                                ) -- importing type
-- end end utility

instance StringLike2PrimitiveM ModelId where
    fromStringM aStr
        | null aStr = throw (EmptyStr "ModelId")
        | not ((isAlphaNumStr aStr) && (isAsciiStr aStr)) = throw
            (NotAsciiAlphanumeric "ModelId")

instance Model2StringTextM ModelId where
