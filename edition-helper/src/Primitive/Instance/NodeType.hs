{-|
Module : NodeId
License : see LICENSE
Description : NodeType primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.NodeType
    ( NodeType
    )
where

-- start def
import           Primitive.Definition.NodeType ( NodeType
                                                    ( StringTypeCons
                                                    , TextTypeCons
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

instance StringLike2Primitive NodeType where
    fromString astr = Right (StringTypeCons astr)

instance TupleString2Primitive NodeType where
    fromTupleString tpl = Right (StringTypeCons (snd tpl))

-- end setter

-- start transformer
instance Model2StringText NodeType where
    toString (StringTypeCons astr) = astr
    toString (TextTypeCons   txt ) = unpack txt
    toText (StringTypeCons astr) = pack astr
    toText (TextTypeCons   txt ) = txt

instance Model2IdTuple NodeType where
    toIdTuple mid = ("type", mid)
-- end transformer
