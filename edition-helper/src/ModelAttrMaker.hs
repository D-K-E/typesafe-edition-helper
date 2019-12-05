module ModelAttrMaker
    ( fromString
    , fromText
    , fromMixedStr
    , fromMixedText
    )
where
-- typeclass for model attr maker

import qualified Data.Text                     as Txt
import           Data.Map                       ( Map
                                                , elems
                                                , keys
                                                )  -- importing type
import           Model                          ( ModelAttrs(..) )
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

class ModelAttrsMaker model where
    fromString :: Map String String -> model
    fromText :: Map Txt.Text Txt.Text -> model
    fromMixedStr :: Map String Txt.Text -> model
    fromMixedText :: Map Txt.Text String -> model

    fromMixedStr aMap = fromText (convertStringKey aMap)
    fromMixedText aMap = fromText (convertStringVal aMap)
    fromText aMap = fromString (convertTxtMap2String aMap)

instance ModelAttrsMaker ModelAttrs where
    fromString aMap
        | all null (elems aMap)
        = error "Attributes must have non empty values"
        | not (all isAlphaNumStr (elems aMap))
        = error "Attributes must have alphanumeric values"
        | not (all isAsciiStr (elems aMap))
        = error "Attributes must have ascii values"
        | not (all isAlphaNumStr (keys aMap))
        = error "Attributes must have alphanumeric keys"
        | not (all isAsciiStr (keys aMap))
        = error "Attributes must have ascii keys"
        | all null (keys aMap)
        = error "Attributes must have non empty keys"
        | otherwise
        = StringAttrsCons aMap
