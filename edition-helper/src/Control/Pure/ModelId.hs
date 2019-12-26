{-|
Module : Model
License : see LICENSE
Description : ModelId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.ModelId
    ( makeModelIdM
    )
where

-- start def
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
import           Primitive.Definition.Error     ( StringValueError(..)
                                                , IdTupleValueError(..)
                                                )
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    , fromText
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



makeModelIdFromString :: String -> Either StringValueError ModelId
makeModelIdFromString astr
    | null astr = Left (EmptyStr "ModelId")
    | not (isAlphaNumStr astr && isAsciiStr astr) = Left
        (NotAsciiAlphanumeric "ModelId")
    | otherwise = fromString astr

makeModelIdFromText :: Text -> Either StringValueError ModelId

makeModelIdFromText txt = makeModelIdFromString (unpack txt)

makeModelIdFromIdTuple :: (String, String) -> Either IdTupleValueError ModelId
makeModelIdFromIdTuple (str1, str2)
    | null str1 = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | str1 != "id" = Left
        (FirstValueError
            (  OtherStringError "IdTuple first argument inappropriate value: "
            ++ str1
            )
        )

makeModelIdFromIdTuple (str1, str2) =
    Left (SecondValueError (Left makeModelIdFromString str2))

makeModelIdFromIdTuple (str1, str2) =
    Right (Right (makeModelIdFromString str2))
