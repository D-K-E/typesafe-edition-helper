{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelId
    ( ModelId
    )
where

-- start def
import           Primitive.Definition.ModelId   ( ModelId
                                                    ( StringIdCons
                                                    , TextIdCons
                                                    )
                                                )
-- end def
-- start fn
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    )
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Pure.Transformer   ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )

-- end fn
-- start utility
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
-- end utility

-- start setter

instance StringLike2Primitive ModelId where
    fromString = StringIdCons

instance TupleString2Primitive ModelId where
    fromTupleString tpl = fromString (snd tpl)

-- end setter

instance Model2IdTuple ModelId where
    toIdTuple mid = ("id", mid)

instance Model2StringText ModelId where
    toString (StringIdCons aModel) = aModel
    toString (TextIdCons   aModel) = unpack aModel
    toText (TextIdCons   aModel) = aModel
    toText (StringIdCons aModel) = pack aModel
