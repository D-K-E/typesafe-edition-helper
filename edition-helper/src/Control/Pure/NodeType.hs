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

import Primitive.Definition.Error
       ( IdTupleValueError (..), TextValueError (..) )
import Primitive.Instance.NodeType ( NodeType )

-- end def
-- start fn
import FunctionDef.Setter
       ( IdTuple2Node (fromTupleString)
       , Text2NodeIdType (fromString, fromText)
       )
-- end fn
-- start utility
import Data.List      ( elem )
import Data.Text      ( Text, pack, unpack )
import Utils.StrUtils ( concatHStr, concatTStr, toLowerTxt, toTxtList )
-- end utility

-- start maker
makeNodeTypeFromText :: Text -> Either TextValueError NodeType

makeNodeTypeFromText typeName
    | toLowerTxt typeName
        `elem` (toTxtList [ "edition", "transliteration", "translation"
                            , "note", "info", "text", "term", "glossary"
                            , "inflected", "attestation", "lemma", "analysis"
                          ]
                )

    = Right (TextTypeCons typeName)
    | otherwise
    = Left (OtherTextError (concatHStr "Unsupported type: " typeName))


-- |'makeNodeTypeFromIdTuple' make model id from id tuple
makeNodeTypeFromIdTuple
    :: (Text, Text) -> Either IdTupleValueError NodeType
makeNodeTypeFromIdTuple (txt1, txt2)
    | null txt1
    = Left (FirstValueError (EmptyTxt (pack "IdTuple first argument")))
    | txt1 /= (pack "type")
    = Left
        (FirstValueError
            (OtherTextError
               (concatHStr
                "IdTuple first argument has inappropriate value: " txt1
                )
            )
        )
    | txt1 == pack "type"
    = let midErr = makeNodeTypeFromText txt2
      in  case midErr of
              Left  err -> Left (SecondTextValueError err)
              Right mid -> Right mid
-- end maker
