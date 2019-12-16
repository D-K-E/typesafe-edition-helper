{-|
Module : ModelType
License : see LICENSE
Description : ModelType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelType
    ( ModelType
    )
where

-- start def
import           Primitive.Definition.ModelType ( ModelType
                                                    ( StringTypeCons
                                                    , TextTypeCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType )
-- end def
-- start functionality
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           Data.List                      ( notElem )
import           Data.Text                      ( Text
                                                , pack
                                                , empty
                                                , unpack
                                                ) -- importing type
import qualified Control.Monad.Fail            as Fail
                                                ( fail )
import           Utils.StrUtils                 ( toLowerStr
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )
-- end functionality

instance StringLikeSetterM ModelType where
    fromStringM typeName
        | toLowerStr typeName
            `notElem` [ "edition"
                      , "transliteration"
                      , "translation"
                      , "note"
                      , "info"
                      , "text"
                      , "term"
                      , "glossary"
                      , "inflected"
                      , "attestation"
                      , "lemma"
                      , "analysis"
                      ]
        = Fail.fail ("unknown model type " ++ typeName)


instance Model2StringTextM ModelType where
