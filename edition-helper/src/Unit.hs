module Unit
    ( UnitModel(UnitCons)
    , ModelInfo(InfoCons)
    , modelId
    , modelType
    , modelAttrs
    , unitInfo
    , unitData
    )
where
-- models of data for making computation easier
import           Data.Map                       ( Map )  -- importing type
import           Data.Text                      ( Text )


class IsUnit model where
    fromTextArgs :: Text -> Map Text Text -> Text -> Text -> model
    fromStringArgs :: String -> Map String String -> String -> String -> model

makeUnitModelFromTextType :: Text -> Map Text Text -> Text -> Text -> UnitModel

makeUnitModel unitId unitAttrs unitType unitData
    | unitType
