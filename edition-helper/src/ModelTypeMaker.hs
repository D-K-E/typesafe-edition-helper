module ModelTypeMaker
    ( fromString
    , fromText
    )
where
-- typeclass for model type maker

import qualified Data.Text                      ( unpack
                                                , Text
                                                )
import           Data.Map                       ( Map )  -- importing type
import           Model                          ( ModelType(..) )
import           Utils                          ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )


class MTypeMaker model where
    fromString :: String -> model
    fromText :: Text -> model
    fromText typeName = fromString (unpack typeName)


instance MTypeMaker ModelType where
    fromString typeName
        | toLowerString typeName == "edition"     = Edition
        | toLowerString typeName == "transliteration" = Transliteration
        | toLowerString typeName == "translation" = Translation
        | toLowerString typeName == "note"        = Note
        | toLowerString typeName == "info"        = Info
        | toLowerString typeName == "text"        = Text
        | toLowerString typeName == "term"        = Term
        | toLowerString typeName == "glossary"    = Glossary
        | toLowerString typeName == "inflected"   = Inflected
        | toLowerString typeName == "attestation" = Attestation
        | toLowerString typeName == "lemma"       = Lemma
        | toLowerString typeName == "analysis"    = Analysis
        | otherwise = error ("unknown model type " ++ typeName)
