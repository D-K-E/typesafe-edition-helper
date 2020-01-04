{-|
Module : NodeId
License : see LICENSE
Description : NodeId primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.NodeId
    ( NodeId
    )
where

-- start def
import           Primitive.Definition.NodeId   ( NodeId
                                                    ( StringIdCons
                                                    , TextIdCons
                                                    )
                                                )
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

instance StringLike2Primitive NodeId where
    fromString astr = Right (StringIdCons astr)

instance TupleString2Primitive NodeId where
    fromTupleString tpl = Right (StringIdCons (snd tpl))

-- end setter

-- start transformer
instance Model2StringText NodeId where
    toString (StringIdCons astr) = astr
    toString (TextIdCons   txt ) = unpack txt
    toText (StringIdCons astr) = pack astr
    toText (TextIdCons   txt ) = txt

instance Model2IdTuple NodeId where
    toIdTuple mid = ("id", mid)
-- end transformer
