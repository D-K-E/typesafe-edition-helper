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
import Primitive.Definition.Node ( NodeType (TextTypeCons) )
-- end def

-- start fn
import FunctionDef.Setter      ( IdTuple2Node (fromTupleString) )
import FunctionDef.Transformer
       ( Model2IdTuple (toIdTuple), NodeIdType2Text (toText) )
-- end fn

-- start utility
import Data.Text ( Text, pack, unpack )
-- end utility

-- start setter


-- end setter

-- start transformer
instance NodeIdType2Text NodeType where
    toText (TextTypeCons txt) = txt

instance Model2IdTuple NodeType where
    toIdTuple mid = (pack "type", mid)
-- end transformer
