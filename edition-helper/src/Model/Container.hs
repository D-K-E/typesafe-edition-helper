{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Model.Container
    ( ContainerModel(..)
    , ContainerData(..)
    )
where

import           Model.ModelInfo                ( ModelInfo )
import           Model.Unit                     ( UnitModel )

-- | container data: container data model which can be either a container
-- model or unit model
data ContainerData = NestedCons ContainerModel
                    | SimpleCons UnitModel
                    deriving (Eq, Show)

-- | container model: container model containing a list of either unit or 
-- container model
data ContainerModel = ContainerCons {
      modelInfo :: ModelInfo
    , modelData :: [ContainerData]
    } deriving (Eq, Show)
