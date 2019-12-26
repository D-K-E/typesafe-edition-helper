{-|
Module : Model
License : see LICENSE
Description : ModelType.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.ModelType
    ( makeModelTypeFromString
    , makeModelTypeFromText
    )
where

-- start def

import           Primitive.Instance.Pure.ModelType
                                                ( ModelType )
import           Primitive.Definition.Error     ( StringValueError(..)
                                                , IdTupleValueError(..)
                                                )

-- end def
-- start fn
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Data.List                      ( elem )
-- end utility


makeModelTypeFromString :: String -> Either StringValueError ModelType
makeModelTypeFromText :: Text -> Either StringValueError ModelType

makeModelTypeFromString typeName
    | toLowerStr typeName
        `elem` [ "edition"
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
    = fromString typeName
    | otherwise
    = Left (OtherStringError "Unsupported type: " ++ typeName)


makeModelTypeFromText txt = makeModelTypeFromString (unpack str)
