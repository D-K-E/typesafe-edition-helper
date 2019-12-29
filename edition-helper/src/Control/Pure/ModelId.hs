{-|
Module : Model
License : see LICENSE
Description : ModelId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.ModelId
    ( makeModelIdFromString
    , makeModelIdFromText
    , makeModelIdFromIdTuple
    )
where

-- start def
import           Primitive.Instance.ModelId     ( ModelId(..) )
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

-- |'makeModelIdFromString' makes model id from string using conditions
makeModelIdFromString :: String -> Either StringValueError ModelId
makeModelIdFromString astr
    | null astr = Left (EmptyStr "ModelId")
    | not (isAlphaNumStr astr && isAsciiStr astr) = Left
        (NotAsciiAlphanumeric "ModelId")
    | otherwise = fromString astr

-- |'makeModelIdFromText' makes model id from text using conditions
makeModelIdFromText :: Text -> Either StringValueError ModelId
makeModelIdFromText txt = makeModelIdFromString (unpack txt)

-- |'makeModelIdFromIdTuple' make model id from id tuple
makeModelIdFromIdTuple :: (String, String) -> Either IdTupleValueError ModelId
makeModelIdFromIdTuple (str1, str2)
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
    = let midErr = makeModelIdFromString str2
      in  case midErr of
              Left  err -> Left (SecondStringValueError err)
              Right mid -> fromTupleString (str1, str2)
