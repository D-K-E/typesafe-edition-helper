{-|
Module : Model
License : see LICENSE
Description : NodeType.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.NodeType
    ( makeNodeTypeFromString
    , makeNodeTypeFromText
    )
where

-- start def

import           Primitive.Instance.NodeType   ( NodeType )
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
makeNodeTypeFromString :: String -> Either StringValueError NodeType
makeNodeTypeFromText :: Text -> Either StringValueError NodeType

makeNodeTypeFromString typeName
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


makeNodeTypeFromText txt = makeNodeTypeFromString (unpack txt)


-- |'makeNodeIdFromIdTuple' make model id from id tuple
makeNodeTypeFromIdTuple
    :: (String, String) -> Either IdTupleValueError NodeType
makeNodeTypeFromIdTuple (str1, str2)
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
    = let midErr = makeNodeTypeFromString str2
      in  case midErr of
              Left  err -> Left (SecondStringValueError err)
              Right mid -> fromTupleString (str1, str2)
-- end maker
