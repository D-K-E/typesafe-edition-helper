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

makeEditionUnit :: Text -> Map Text Text -> Text -> UnitModel

makeEditionUnit editionId editionAttrs editionData = UnitCons
    { unitInfo = InfoCons { modelId    = editionId
                          , modelType  = Text.pack "edition-unit"
                          , modelAttrs = editionAttrs
                          }
    , unitData = editionData
    }

makeTransliterationUnit :: Text -> Map Text Text -> Text -> UnitModel
makeTransliterationUnit transliterationId transliterationAttrs transliterationData
    = UnitCons
        { unitInfo = InfoCons { modelId    = transliterationId
                              , modelType  = Text.pack "transliteration-unit"
                              , modelAttrs = transliterationAttrs
                              }
        , unitData = transliterationData
        }

makeTranslationUnit :: Text -> Map Text Text -> Text -> UnitModel
makeTranslationUnit translationId translationAttrs translationData = UnitCons
    { unitInfo = InfoCons { modelId    = translationId
                          , modelType  = Text.pack "translation-unit"
                          , modelAttrs = translationAttrs
                          }
    , unitData = translationData
    }

makeNoteUnit :: Text -> Map Text Text -> Text -> UnitModel
makeNoteUnit noteId noteAttrs noteData = UnitCons
    { unitInfo = InfoCons { modelId    = noteId
                          , modelType  = Text.pack "note-unit"
                          , modelAttrs = noteAttrs
                          }
    , unitData = noteData
    }
