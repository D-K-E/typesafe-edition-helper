{-|
Module : ModelType
License : see LICENSE
Description : ModelType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelType
    ( ModelType
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( toLowerStr
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )

import           FunctionDef.Setter             ( StringLikeSetter
                                                , fromString
                                                )
import           Primitive.Definition.ModelType ( ModelType
                                                    ( StringTypeCons
                                                    , TextTypeCons
                                                    )
                                                )
import           View.Transformer               ( Model2StringText
                                                , toString
                                                , toText
                                                )

instance StringLikeSetter ModelType where
    fromString typeName
        | toLowerStr typeName == "edition" = return StringTypeCons "edition"
        | toLowerStr typeName == "transliteration" = return
            StringTypeCons
            "transliteration"
        | toLowerStr typeName == "translation" = return StringTypeCons
                                                        "translation"
        | toLowerStr typeName == "note" = return StringTypeCons "note"
        | toLowerStr typeName == "info" = return StringTypeCons "info"
        | toLowerStr typeName == "text" = return StringTypeCons "text"
        | toLowerStr typeName == "term" = return StringTypeCons "term"
        | toLowerStr typeName == "glossary" = return StringTypeCons "glossary"
        | toLowerStr typeName == "inflected" = return StringTypeCons "inflected"
        | toLowerStr typeName == "attestation" = return StringTypeCons
                                                        "attestation"
        | toLowerStr typeName == "lemma" = return StringTypeCons "lemma"
        | toLowerStr typeName == "analysis" = return StringTypeCons "analysis"
        | otherwise = fail ("unknown model type " ++ typeName)


instance Model2StringText ModelType where
    toString (StringTypeCons aModel) = aModel
    toString (TextTypeCons   aModel) = unpack aModel
    toText (StringTypeCons aModel) = pack aModel
    toText (TextTypeCons   aModel) = aModel
