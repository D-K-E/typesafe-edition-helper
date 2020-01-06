{-|
Module : Model
License : see LICENSE
Description : NodeId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.NodeId
    ( makeNodeIdFromString
    , makeNodeIdFromText
    , makeNodeIdFromIdTuple
    )
where

-- start def
import           Primitive.Instance.NodeId     ( NodeId(..) )
import           Primitive.Definition.Error     ( TextValueError(..)
                                                , IdTupleValueError(..)
                                                )
import           FunctionDef.Setter             ( Text2NodeIdType
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , IdTuple2Node
                                                    ( fromTupleString
                                                    )
                                                )
-- end def
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type

-- start maker

-- |'makeNodeIdFromString' makes model id from string using conditions
makeNodeIdFromString :: String -> Either TextValueError NodeId
makeNodeIdFromString astr
    | null astr = Left (EmptyStr "NodeId")
    | not (isAlphaNumStr astr && isAsciiStr astr) = Left
        (NotAsciiAlphanumeric "NodeId")
    | otherwise = fromString astr

-- |'makeNodeIdFromText' makes model id from text using conditions
makeNodeIdFromText :: Text -> Either TextValueError NodeId
makeNodeIdFromText txt = makeNodeIdFromString (unpack txt)

-- |'makeNodeIdFromIdTuple' make model id from id tuple
makeNodeIdFromIdTuple :: (String, String) -> Either IdTupleValueError NodeId
makeNodeIdFromIdTuple (str1, str2)
    | null str1
    = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | not (str1 == "id")
    = Left
        (FirstValueError
            (OtherStringError
                ("IdTuple first argument has inappropriate value: " ++ str1)
            )
        )
    | str1 == "id"
    = let midErr = makeNodeIdFromString str2
      in  case midErr of
              Left  err -> Left (SecondTextValueError err)
              Right mid -> fromTupleString (str1, str2)
