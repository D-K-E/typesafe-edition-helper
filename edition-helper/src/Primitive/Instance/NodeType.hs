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
import           Primitive.Definition.NodeType  ( NodeType(TextTypeCons) )
-- end def

-- start fn
import           FunctionDef.Setter             ( IdTuple2Node(fromTupleString)
                                                )
import           FunctionDef.Transformer        ( NodeIdType2Text(toText)
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

instance IdTuple2Node NodeType where
    fromTupleString tpl = Right (StringTypeCons (snd tpl))

-- end setter

-- start transformer
instance NodeIdType2Text NodeType where
    toText (TextTypeCons txt) = txt

instance Model2IdTuple NodeType where
    toIdTuple mid = ("type", mid)
-- end transformer
