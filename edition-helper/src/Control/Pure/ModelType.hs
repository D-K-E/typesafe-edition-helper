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

import           Primitive.Instance.ModelType   ( ModelType )
import           Primitive.Definition.Error     ( StringValueError(..)
                                                , IdTupleValueError(..)
                                                )

-- end def
-- start fn
import           FunctionDef.Setter             ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , TupleString2Primitive
                                                    ( fromTupleString
                                                    )
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Data.List                      ( elem )
import           Utils.StrUtils                 ( toLowerStr )
-- end utility

-- start maker
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
    = Left (OtherStringError ("Unsupported type: " ++ typeName))


makeModelTypeFromText txt = makeModelTypeFromString (unpack txt)


-- |'makeModelIdFromIdTuple' make model id from id tuple
makeModelTypeFromIdTuple
    :: (String, String) -> Either IdTupleValueError ModelType
makeModelTypeFromIdTuple (str1, str2)
    | null str1
    = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | not (str1 == "type")
    = Left
        (FirstValueError
            (OtherStringError
                ("IdTuple first argument has inappropriate value: " ++ str1)
            )
        )
    | str1 == "type"
    = let midErr = makeModelTypeFromString str2
      in  case midErr of
              Left  err -> Left (SecondStringValueError err)
              Right mid -> fromTupleString (str1, str2)
-- end maker
