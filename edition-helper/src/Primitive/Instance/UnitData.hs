{-|
Module : NodeId
License : see LICENSE
Description : UnitData primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.UnitData
    ( UnitData
    )
where

-- start def
import           Primitive.Definition.UnitData  ( UnitData
                                                    ( StringUnitDataCons
                                                    , TextUnitDataCons
                                                    )
                                                )
-- end def

-- start fn
import           FunctionDef.Setter             ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , IdTuple2Node
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

instance StringLike2Primitive UnitData where
    fromString astr = Right (StringUnitDataCons astr)

instance IdTuple2Node UnitData where
    fromTupleString tpl = Right (StringUnitDataCons (snd tpl))

-- end setter

-- start transformer
instance Model2StringText UnitData where
    toString (StringUnitDataCons astr) = astr
    toString (TextUnitDataCons   txt ) = unpack txt
    toText (StringUnitDataCons astr) = pack astr
    toText (TextUnitDataCons   txt ) = txt

instance Model2IdTuple UnitData where
    toIdTuple udata = ("udata", udata)
-- end transformer
