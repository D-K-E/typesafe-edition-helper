{-|
Module : Matcher.hs
License : see LICENSE
Description : Contains typeclasses that are related to matching primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Matcher where

import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )
import           Primitive.Definition.ModelData ( ModelData )


-- | 'MatchModel' class outlines methods to match model to given field
class MatchModel model where
    -- |'hasSameId' whether id of given model is same with given model id
    hasSameId :: model -> ModelId -> Bool

    -- |'hasSameType' whether type of given model is same with given model type
    hasSameType :: model -> ModelType -> Bool

    -- |'hasSameAttr' whether Attr of given model is same with given model Attr
    hasSameAttr :: model -> ModelAttr -> Bool

    -- |'hasSameData' whether Data of given model is same with given model Data
    hasSameData :: model -> ModelData -> Bool

    -- |'containsId' whether model id of model contains given ModelId
    containsId :: model -> ModelId -> Bool

    -- |'containsType' whether model id of model contains given ModelType
    containsType :: model -> ModelType -> Bool

    -- |'containsAttr' whether model id of model contains given ModelAttr
    containsAttr :: model -> ModelAttr -> Bool

    -- |'containsData' whether Data of given model contains with given model Data
    containsData :: model -> ModelData -> Bool
