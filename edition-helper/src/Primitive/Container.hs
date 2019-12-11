{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Model.Container where

-- definition related imports
import           Model.ModelInfo                ( ModelInfo )
import           Model.Unit                     ( UnitModel )
import           Model.ModelData                ( CData )
-- end of definition related imports

import           View.Transformer               ( Model2Tuple(..) )

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

instance Model2Tuple ContainerModel where
    toTuple model = (modelInfo model, CData (modelData model))
