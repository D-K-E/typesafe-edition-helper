{-|
Module : Modifier.hs
License : see LICENSE
Description : Contains typeclasses that are related to modifying primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Modifier
    ( ReplaceInfoField(..)
    , ReplaceField(..)
    , Add2Field(..)
    )
where

import           Primitive.Definition.NodeId   ( NodeId )
import           Primitive.Definition.NodeType ( NodeType )
import           Primitive.Definition.NodeAttr ( NodeAttr )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.NodeInfo ( NodeInfo )


-- | 'ReplaceInfoField' class outlines methods to replace model info fields
class ReplaceInfoField model where

    -- |'replaceId' replaces id of model with given NodeId
    replaceId :: model -> NodeId -> model
    -- |'replaceType' replaces Type of model with given NodeType
    replaceType :: model -> NodeType -> model
    -- |'replaceAttr' replaces Attr of model with given NodeAttr
    replaceAttr :: model -> NodeAttr -> model


-- | 'ReplaceField' class outlines methods to replace model to given field
class (ReplaceInfoField model) => ReplaceField model where

    -- |'replaceInfo' replaces Info of model with given NodeInfo
    replaceInfo :: model -> NodeInfo -> model
    -- |'replaceData' replaces Data of model with given ModelData
    replaceData :: model -> ModelData -> model

-- | 'Add2Field' class outlines methods to add new value to given field
class (ReplaceField model) => Add2Field model where
    -- |'append2Id' append new model id at the end of model id
    append2Id :: model -> NodeId -> model

    -- |'prepend2Id' prepend new model id at the start of model id
    prepend2Id :: model -> NodeId -> model

    -- |'append2Type' append new model Type at the end of model Type
    append2Type :: model -> NodeType -> model

    -- |'prepend2Type' prepend new model Type at the start of model Type
    prepend2Type :: model -> NodeType -> model

    -- |'append2Attr' append new model Attr at the end of model Attr
    add2Attr :: model -> NodeAttr -> model

    -- |'append2Data' append new model Data at the end of model Data
    append2Data :: model -> ModelData -> model

    -- |'prepend2Data' prepend new model Data at the start of model Data
    prepend2Data :: model -> ModelData -> model
