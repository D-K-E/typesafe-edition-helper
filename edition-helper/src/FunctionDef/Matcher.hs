{-|
Module : Matcher.hs
License : see LICENSE
Description : Contains typeclasses that are related to matching primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Matcher
    ( MatchModel(..)
    )
where

import           Primitive.Definition.NodeId   ( NodeId )
import           Primitive.Definition.NodeType ( NodeType )
import           Primitive.Definition.NodeAttr ( NodeAttr )
import           Primitive.Definition.ModelData ( ModelData )


-- | 'MatchModel' class outlines methods to match model to given field
class MatchModel model where
    -- |'hasSameId' whether id of given model is same with given model id
    hasSameId :: model -> NodeId -> Bool

    -- |'hasSameType' whether type of given model is same with given model type
    hasSameType :: model -> NodeType -> Bool

    -- |'hasSameAttr' whether Attr of given model is same with given model Attr
    hasSameAttr :: model -> NodeAttr -> Bool

    -- |'hasSameData' whether Data of given model is same with given model Data
    hasSameData :: model -> ModelData -> Bool

    -- |'containsId' whether model id of model contains given NodeId
    containsId :: model -> NodeId -> Bool

    -- |'containsType' whether model id of model contains given NodeType
    containsType :: model -> NodeType -> Bool

    -- |'containsAttr' whether model id of model contains given NodeAttr
    containsAttr :: model -> NodeAttr -> Bool

    -- |'containsData' whether Data of given model contains with given model Data
    containsData :: model -> ModelData -> Bool
