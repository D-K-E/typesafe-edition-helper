{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.ModelId where

-- start def
import           Primitive.Definition.ModelId   ( ModelId(StringIdCons) )
-- end def

-- start fn
import           FunctionDef.Setter             ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , TupleString2Primitive
                                                    ( fromTupleString
                                                    )
                                                )
import           FunctionDef.Transformer        ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )
-- end fn

-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
-- end utility

-- start setter

instance StringLike2Primitive ModelId where
    fromString astr = Right (StringIdCons astr)

instance TupleString2Primitive ModelId where
    fromTupleString tpl = Right (StringIdCons (snd tpl))

-- end setter

-- start transformer
instance Model2StringText ModelId where
    toString (StringIdCons astr) = astr
    toString (TextIdCons   txt ) = unpack txt
    toText (StringIdCons astr) = pack astr
    toText (TextIdCons   txt ) = txt

instance Model2IdTuple ModelId where
    toIdTuple mid = ("id", mid)
-- end transformer
