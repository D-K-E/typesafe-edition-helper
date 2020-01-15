{-|
Module : Model
License : see LICENSE
Description : UnitData control structure
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.UnitData
    ( makeUnitDataFromString
    , makeUnitDataFromText
    , makeUnitDataFromIdTuple
    )
where

-- start def
import FunctionDef.Setter
       ( IdTuple2Node (fromTupleString)
       , Text2NodeIdType (fromString, fromText)
       )
import Primitive.Definition.Error
       ( IdTupleValueError (..), TextValueError (..) )
import Primitive.Definition.UnitData ( UnitData (..) )
import Primitive.Instance.UnitData   ( UnitData )
-- end def
import Data.Text      ( Text, pack, unpack )
import Utils.StrUtils ( isAlphaNumStr, isAsciiStr )

-- start maker

-- |'makeUnitDataFromString' makes model id from string using conditions
makeUnitDataFromString :: String -> Either TextValueError UnitData
makeUnitDataFromString astr | null astr = Left (EmptyStr "UnitData")
                            | otherwise = fromString astr

-- |'makeUnitDataFromText' makes model id from text using conditions
makeUnitDataFromText :: Text -> Either TextValueError UnitData
makeUnitDataFromText txt = makeUnitDataFromString (unpack txt)

-- |'makeUnitDataFromIdTuple' make model id from id tuple
makeUnitDataFromIdTuple :: (String, String) -> Either IdTupleValueError UnitData
makeUnitDataFromIdTuple (str1, str2)
    | null str1
    = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | not (str1 == "udata")
    = Left
        (FirstValueError
            (OtherStringError
                ("IdTuple first argument has inappropriate value: " ++ str1)
            )
        )
    | str1 == "udata"
    = let midErr = makeUnitDataFromString str2
      in  case midErr of
              Left  err -> Left (SecondTextValueError err)
              Right mid -> fromTupleString (str1, str2)
