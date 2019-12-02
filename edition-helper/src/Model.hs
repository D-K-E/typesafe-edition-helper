module Model
    ( UnitModel
    , ContainerData
    , ContainerModel
    , ModelInfo
    , modelId
    , modelType
    , modelAttrs
    , unitInfo
    , containerInfo
    , unitData
    , containerData
    , fromContainerModel
    , fromUnitModel
    )
where
-- models of data for making computation easier
import           Data.Map                       ( Map )  -- importing type
import           Data.Text                      ( Text )



data ModelInfo = InfoCons {
                    modelId :: Text, modelType :: Text,
                    modelAttrs :: Map Text Text
                } deriving (Show, Eq)

data UnitModel = UnitCons { unitInfo :: ModelInfo, unitData :: Text} deriving (Show, Eq)

data ContainerModel = ContainerCons {  containerInfo :: ModelInfo
                       , containerData :: [ContainerData]} deriving (Show, Eq)


data ContainerData = NestedCons ContainerModel
                    | SimpleCons UnitModel
                     deriving (Show, Eq)

fromContainerModel :: ContainerModel -> ContainerData
fromContainerModel cm = NestedCons cm
fromUnitModel :: UnitModel -> ContainerData
fromUnitModel cm = SimpleCons cm
