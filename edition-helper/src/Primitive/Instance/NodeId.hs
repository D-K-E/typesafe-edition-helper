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
import           Primitive.Definition.NodeId    ( NodeId(TextIdCons) )
-- end def

-- start fn
import           FunctionDef.Setter             ( Text2NodeIdType
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , IdTuple2Node(fromTupleString)
                                                )
import           FunctionDef.Transformer        ( NodeIdType2Text
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

instance IdTuple2Node NodeId where
    fromTupleString tpl = Right (StringIdCons (snd tpl))

-- end setter

-- start transformer
instance NodeIdType2Text NodeId where
    toString (StringIdCons astr) = astr
    toString (TextIdCons   txt ) = unpack txt
    toText (StringIdCons astr) = pack astr
    toText (TextIdCons   txt ) = txt

instance Model2IdTuple NodeId where
    toIdTuple mid = ("id", mid)
-- end transformer
