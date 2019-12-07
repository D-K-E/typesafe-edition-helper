{-|
Module : ModelType
License : see LICENSE
Description : ModelType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module ModelType
    ( ModelType(..)
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )

import           Utils.DataUtils                ( StringLikeCons
                                                , Model2StringText
                                                )

-- | model type: edition, inflected, glossary it can be constructed from string
data ModelType = TextTypeCons Text
                | StringTypeCons String
                deriving (Eq, Show)

instance StringLikeCons ModelType where
    fromString typeName
        | toLowerString typeName == "edition" = StringTypeCons "edition"
        | toLowerString typeName == "transliteration" = StringTypeCons
            "transliteration"
        | toLowerString typeName == "translation" = StringTypeCons "translation"
        | toLowerString typeName == "note" = StringTypeCons "note"
        | toLowerString typeName == "info" = StringTypeCons "info"
        | toLowerString typeName == "text" = StringTypeCons "text"
        | toLowerString typeName == "term" = StringTypeCons "term"
        | toLowerString typeName == "glossary" = StringTypeCons "glossary"
        | toLowerString typeName == "inflected" = StringTypeCons "inflected"
        | toLowerString typeName == "attestation" = StringTypeCons "attestation"
        | toLowerString typeName == "lemma" = StringTypeCons "lemma"
        | toLowerString typeName == "analysis" = StringTypeCons "analysis"
        | otherwise = error ("unknown model type " ++ typeName)


instance Model2StringText ModelType where
    toString (StringTypeCons aModel) = aModel
    toString (TextTypeCons   aModel) = unpack aModel
    toText (StringTypeCons aModel) = pack aModel
    toText (TextTypeCons   aModel) = aModel
