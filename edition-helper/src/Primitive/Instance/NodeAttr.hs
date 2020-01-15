{-|
Module : NodeId
License : see LICENSE
Description : NodeAttr primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.NodeAttr
    ( NodeAttr
    )
where

-- start def
import Primitive.Definition.Error
       ( IdTupleValueError, MapValueError (..), TextValueError )
import Primitive.Definition.Node  ( NodeAttr (TextAttrCons) )
-- end def

-- start fn
import FunctionDef.Setter
       ( Map2Primitive (fromStringMap)
       , TupleMap2Primitive (fromTupleStringMap)
       )
import FunctionDef.Transformer
       ( Model2IdTuple (toIdTuple), Model2Map (toStringMap, toTextMap) )
-- end fn

-- start utility
import Data.Map.Strict ( Map )
import Data.Text       ( Text )
import Utils.MapUtils  ( convertStringMap2Txt, convertTxtMap2String )
-- end utility

-- setter

instance Map2Primitive NodeAttr where
    fromTextMap amap = Right (TextAttrCons amap)

-- end setter
-- transformer

instance Model2Map NodeAttr where
    toTextMap (TextAttrCons   amap) = amap

-- end transformer
