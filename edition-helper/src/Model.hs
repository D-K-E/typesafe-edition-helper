module Model
    ( UnitModel
    , ContainerData(..)
    , ContainerModel
    , ModelInfo(..)
    , modelId
    , modelType
    , modelAttrs
    , unitInfo
    , containerInfo
    , unitData
    , containerData
    )
where
-- models of data for making computation easier
import           Data.Map                       ( Map )  -- importing type
import           Data.Text                      ( Text )

data ModelType = Edition
                | Transliteration
                | Translation
                | Note
                | Info
                | Text
                | Term
                | Glossary
                | Inflected
                | Attestation
                | Lemma
                | Analysis

data ModelInfo = InfoCons {
                    modelId :: Text, modelType :: ModelType,
                    modelAttrs :: Map Text Text
                } deriving (Show, Eq)

data UnitModel = UnitCons { unitInfo :: ModelInfo, unitData :: Text} deriving (Show, Eq)

data ContainerModel = ContainerCons {  containerInfo :: ModelInfo
                       , containerData :: [ContainerData]} deriving (Show, Eq)


data ContainerData = NestedCons ContainerModel
                    | SimpleCons UnitModel
                     deriving (Show, Eq)
