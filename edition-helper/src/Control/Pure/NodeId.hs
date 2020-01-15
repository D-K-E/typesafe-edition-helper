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
import FunctionDef.Setter
       ( IdTuple2Node (fromTupleString), Text2NodeIdType (fromText) )
import Primitive.Definition.Error
       ( IdTupleValueError (..), TextValueError (..) )
import Primitive.Instance.NodeId  ( NodeId (..) )
-- end def
import Data.Text      ( Text, pack, unpack )
import Utils.StrUtils ( isAlphaNumStr, isAsciiStr )

-- start maker

-- |'makeNodeIdFromString' makes model id from string using conditions
makeNodeIdFromString :: String -> Either TextValueError NodeId
makeNodeIdFromString astr
    | null astr = Left (EmptyTxt . pack "NodeId")
    | not (isAlphaNumStr astr && isAsciiStr astr) = Left
        (NotAsciiAlphanumeric . pack "NodeId")
    | otherwise = fromText (pack astr)

-- |'makeNodeIdFromText' makes model id from text using conditions
makeNodeIdFromText :: Text -> Either TextValueError NodeId
makeNodeIdFromText txt = makeNodeIdFromString (unpack txt)

-- |'makeNodeIdFromIdTuple' make model id from id tuple
makeNodeIdFromIdTuple :: (String, String) -> Either IdTupleValueError NodeId
makeNodeIdFromIdTuple (str1, str2)
    | null str1
    = Left (FirstValueError (EmptyTxt . pack "IdTuple first argument"))
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
