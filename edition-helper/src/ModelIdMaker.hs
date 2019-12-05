module ModelIdMaker
    ( fromString
    , fromText
    )
where
-- typeclass for model type maker

import qualified Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Map                       ( Map )  -- importing type
import           Model                          ( ModelId(..) )
import           Utils                          ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )

class MIdMaker model where
    fromString :: String -> model
    fromText :: Text -> model


instance MIdMaker ModelId where
    fromString aStr
        | null aStr
        = error "empty string is not allowed as id"
        | not (isAlphaNumStr aStr)
        = error "Only ascii alphanumeric strings are allowed"
        | not (isAsciiStr aStr)
        = error "Only ascii alphanumeric strings are allowed"
        | otherwise
        = StringIdCons aStr
    fromText aText = fromString (unpack aText)
