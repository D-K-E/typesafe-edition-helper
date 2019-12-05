module ModelInfoMaker
    ( fromString
    , fromText
    , fromIdStr
    , fromTypeStr
    , fromStringKeyMap
    , fromStringValMap
    , fromStringKeyValMap
    , fromIdTxt
    , fromTypeTxt
    , fromTxtKeyMap
    , fromTxtValMap
    , fromTxtKeyValMap
    )
where
-- type class for model info
import           Utils                          ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                , convertStringKey
                                                , convertStringVal
                                                , convertStringMap2Txt
                                                , convertTxtMap2String
                                                , convertTxtKey
                                                , convertTxtVal
                                                )
import           Model                          ( ModelType
                                                , ModelId(..)
                                                , ModelAttrs(..)
                                                )
import qualified ModelAttrMaker                as MaMaker
import qualified ModelIdMaker                  as MiMaker
import qualified ModelTypeMaker                as MtMaker
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Text                     as Txt
                                                ( Text
                                                , pack
                                                , unpack
                                                )


class MInfoMaker model where
    fromString :: String -> String -> Map String String -> model
    fromText :: Txt.Text -> Txt.Text -> Map Txt.Text Txt.Text -> model

    fromIdStr :: String -> Txt.Text -> Map Txt.Text Txt.Text -> model
    fromTypeStr :: Txt.Text -> String -> Map Txt.Text Txt.Text -> model

    fromStringKeyMap :: Txt.Text -> Txt.Text -> Map String Txt.Text -> model
    fromStringValMap :: Txt.Text -> Txt.Text -> Map Txt.Text String -> model
    fromStringKeyValMap :: Txt.Text -> Txt.Text -> Map String String -> model

    fromIdTxt :: Txt.Text -> String -> Map String String -> model
    fromTypeTxt :: String -> Txt.Text -> Map String String -> model
    fromTxtKeyMap :: String -> String -> Map Txt.Text String -> model
    fromTxtValMap :: String -> String -> Map String Txt.Text -> model
    fromTxtKeyValMap :: String -> String -> Map Txt.Text Txt.Text -> model

    fromText modelId modelType modelAttr =
        fromString (Txt.unpack modelId) (Txt.unpack modelType)
        (convertTxtMap2String modelAttr)

    fromIdStr modelId = fromText (Txt.pack modelId)
    fromTypeStr modelId modelType = fromText modelId (Txt.pack modelType)

    fromStringKeyMap mId mType mAttr =
        fromText mId mType (convertStringKey mAttr)

    fromStringValMap mId mType mAttr =
        fromText mId mType (convertStringVal mAttr)

    fromStringKeyValMap mId mType mAttr =
        fromText mId mType (convertStringMap2Txt mAttr)

    fromIdTxt mid = fromString (Txt.unpack mid)

    fromTypeTxt mid mtype = fromString  mid (Txt.unpack mtype)

    fromTxtKey mid mtype mattr =
        fromString  mid mtype (convertTxtKey mattr)

    fromTxtVal mid mtype mattr =
        fromString  mid mtype (convertTxtVal mattr)

    fromTxtKeyVal mid mtype mattr =
        fromString  mid mtype (convertTxtMap2String mattr)


-- instances


instance ModelInfoMaker ModelInfo where
    fromString modelId modelType modelAttrs = InfoCons
        { modelId    = MiMaker.fromString modelId
        , modelType  = MtMaker.fromString modelType
        , modelAttrs = MaMaker.fromString modelAttrs
        }
