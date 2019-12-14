{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Definition.Container
    ( ContainerData(..)
    , ContainerModel(..)
    )
where

-- definition related imports
import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.Unit      ( UnitModel )
-- end of definition related imports

-- | container data: container data model which can be either a container
-- model or unit model
data ContainerData = CModel ContainerModel
                    | UModel UnitModel
                    deriving (Eq, Show)

-- | container model: container model containing a list of either unit or 
-- container model
data ContainerModel = ContainerCons {
      modelInfo :: ModelInfo
    , modelData :: [ContainerData]
    } deriving (Eq, Show)
