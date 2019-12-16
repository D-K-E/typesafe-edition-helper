{-|
Module : ModelType
License : see LICENSE
Description : ModelType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelType
    ( ModelType
    )
where

-- start def

import           Primitive.Definition.ModelType ( ModelType
                                                    ( StringTypeCons
                                                    , TextTypeCons
                                                    )
                                                )

-- end def

-- start fn

import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    )
                                                )
import           FunctionDef.Pure.Transformer   ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )

-- end fn

-- start utility

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( toLowerStr
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )

-- end utility

instance Model2IdTuple ModelType where
    toIdTuple mtype = ("model-type", mtype)

instance StringLikeSetter ModelType where
    fromString typeName
        | toLowerStr typeName == "edition" = StringTypeCons "edition"
        | toLowerStr typeName == "transliteration" = StringTypeCons
            "transliteration"
        | toLowerStr typeName == "translation" = StringTypeCons "translation"
        | toLowerStr typeName == "note" = StringTypeCons "note"
        | toLowerStr typeName == "info" = StringTypeCons "info"
        | toLowerStr typeName == "text" = StringTypeCons "text"
        | toLowerStr typeName == "term" = StringTypeCons "term"
        | toLowerStr typeName == "glossary" = StringTypeCons "glossary"
        | toLowerStr typeName == "inflected" = StringTypeCons "inflected"
        | toLowerStr typeName == "attestation" = StringTypeCons "attestation"
        | toLowerStr typeName == "lemma" = StringTypeCons "lemma"
        | toLowerStr typeName == "analysis" = StringTypeCons "analysis"


instance Model2StringText ModelType where
    toString (StringTypeCons aModel) = aModel
    toString (TextTypeCons   aModel) = unpack aModel
    toText (StringTypeCons aModel) = pack aModel
    toText (TextTypeCons   aModel) = aModel
