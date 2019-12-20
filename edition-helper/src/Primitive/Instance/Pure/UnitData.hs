{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.UnitData
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
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
-- end utility

-- start setter

instance StringLike2Primitive UnitData where
    fromString = StringUnitDataCons

instance TupleString2Primitive UnitData where
    fromTupleString tpl = fromString (snd tpl)

-- end setter

instance Model2IdTuple UnitData where
    toIdTuple udata = ("data-unit", udata)

instance Model2StringText UnitData where
    toString (StringUnitDataCons ud) = ud
    toString (TextUnitDataCons   ud) = unpack ud
    toText (StringUnitDataCons ud) = pack ud
    toText (TextUnitDataCons   ud) = ud
