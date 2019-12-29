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
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData(..) )
import           Primitive.Definition.Error     ( StringValueError(..)
                                                , IdTupleValueError(..)
                                                )
import           FunctionDef.Setter             ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , TupleString2Primitive
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

-- |'makeUnitDataFromString' makes model id from string using conditions
makeUnitDataFromString :: String -> Either StringValueError UnitData
makeUnitDataFromString astr | null astr = Left (EmptyStr "UnitData")
                            | otherwise = fromString astr

-- |'makeUnitDataFromText' makes model id from text using conditions
makeUnitDataFromText :: Text -> Either StringValueError UnitData
makeUnitDataFromText txt = makeUnitDataFromString (unpack txt)

-- |'makeUnitDataFromIdTuple' make model id from id tuple
makeUnitDataFromIdTuple :: (String, String) -> Either IdTupleValueError UnitData
makeUnitDataFromIdTuple (str1, str2)
    | null str1
    = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | str1 != "udata"
    = Left
        (FirstValueError
            (OtherStringError "IdTuple first argument has inappropriate value: "
            ++ str1
            )
        )
    | str1 == "udata"
    = let midErr = makeUnitDataFromString str2
      in  case midErr of
              Left  err -> Left (SecondValueError (Left err))
              Right mid -> fromTupleString (str1, str2)
