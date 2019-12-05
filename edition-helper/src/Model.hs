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
    , ModelType
    , ModelId(..)
    , ModelAttrs(..)
    )
where
-- models of data for making computation easier
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Text                     as Txt
                                                ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.String
import           Utils                          ( toLowerString )


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
                deriving (Show, Eq)

data ModelId = TextIdCons Txt.Text
               | StringIdCons String
               deriving (Eq, Show)

data ModelAttrs = TextAttrsCons Map Txt.Text Txt.Text
                  | StringAttrsCons Map String String
                  deriving (Eq, Show)


data ModelInfo = InfoCons {
                    modelId :: ModelId, modelType :: ModelType,
                    modelAttrs :: ModelAttrs
                    } deriving (Show, Eq)

data UnitModel = UnitCons { unitInfo :: ModelInfo,
                            unitData :: Txt.Text } deriving (Show, Eq)

data ContainerModel = ContainerCons { containerInfo :: ModelInfo
                                    , containerData :: [ContainerData]
                                    } deriving (Show, Eq)


data ContainerData = NestedCons ContainerModel
                    | SimpleCons UnitModel
                     deriving (Show, Eq)
