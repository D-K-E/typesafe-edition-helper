{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Container
    ( ContainerModel
    )
where

import           Model.ModelInfo                ( ModelInfo )
import           Model.ContainerData            ( ContainerData(..) )

-- | container model: container model containing a list of either unit or 
-- container model
data ContainerModel = ContainerCons {
    modelInfo :: ModelInfo
    , containerData :: [ContainerData]
    } deriving (Eq, Show)
