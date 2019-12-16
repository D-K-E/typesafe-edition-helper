{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelData
    ( ModelData
    )
where

import           Primitive.Definition.ModelData ( ModelData(UData, CData) )
import           Primitive.Definition.UnitData  ( UnitData
                                                    ( StringUnitDataCons
                                                    , TextUnitDataCons
                                                    )
                                                )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData )
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                , fromString
                                                , TupleString2Primitive(..)
                                                )
import           FunctionDef.Pure.Transformer   ( Model2IdTuple(toIdTuple) )

-- start setter
instance StringLike2Primitive ModelData where
    fromString astr = UData (StringUnitDataCons astr)

instance TupleString2Primitive ModelData where
    fromTupleString tpl = fromString (snd tpl)

-- end setter

instance Model2IdTuple ModelData where
    toIdTuple (UData mdata) = ("data-unit", UData mdata)
    toIdTuple (CData mdata) = ("data-container", CData mdata)
