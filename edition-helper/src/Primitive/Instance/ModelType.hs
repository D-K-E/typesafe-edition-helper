{-|
Module : ModelId
License : see LICENSE
Description : ModelType primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelType where

-- start def
import           Primitive.Definition.ModelType ( ModelType(StringTypeCons) )
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

instance StringLike2Primitive ModelType where
    fromString astr = Right (StringTypeCons astr)

instance TupleString2Primitive ModelType where
    fromTupleString tpl = Right (StringTypeCons (snd tpl))

-- end setter

-- start transformer
instance Model2StringText ModelType where
    toString (StringTypeCons astr) = astr
    toString (TextTypeCons   txt ) = unpack txt
    toText (StringTypeCons astr) = pack astr
    toText (TextTypeCons   txt ) = txt

instance Model2IdTuple ModelType where
    toIdTuple mid = ("type", mid)
-- end transformer
