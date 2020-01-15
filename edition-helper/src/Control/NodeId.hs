{-|
Module : Model
License : see LICENSE
Description : NodeId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.NodeId
    ( makeNodeIdFromText
    , makeNodeIdFromIdTuple
    )
where

-- start def
import           FunctionDef.Setter             ( IdTuple2Node(fromTupleString)
                                                )
import           Primitive.Definition.Error     ( IdTupleValueError(..)
                                                , TextValueError(..)
                                                )
import           Primitive.Instance.NodeId
import           Primitive.Definition.Node      ( NodeId(TextIdCons) )
-- end def
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
                                                ( null )
import           Utils.StrUtils                 ( isAlphaNumText
                                                , isAsciiTxt
                                                , concatHStr
                                                , concatTStr
                                                )

-- start maker

-- |'makeNodeIdFromText' makes model id from string using conditions
makeNodeIdFromText :: Text -> Either TextValueError NodeId
makeNodeIdFromText astr
    | T.null astr = Left (EmptyText (pack "NodeId"))
    | not (isAlphaNumText astr && isAsciiTxt astr) = Left
        (NotAsciiAlphanumeric (pack "NodeId"))
    | otherwise = Right (TextIdCons astr)

-- |'makeNodeIdFromIdTuple' make model id from id tuple
makeNodeIdFromIdTuple :: (Text, Text) -> Either IdTupleValueError NodeId
makeNodeIdFromIdTuple (txt1, txt2)
    | T.null txt1
    = Left (FirstValueError (EmptyText (pack "IdTuple first argument")))
    | txt1 /= pack "id"
    = Left
        (FirstValueError
            (OtherTextError
                (concatHStr
                    "IdTuple first argument has inappropriate value: "
                    txt1
                )
            )
        )
    | txt1 == pack "id"
    = let midErr = makeNodeIdFromText txt2
      in  case midErr of
              Left  err -> Left (SecondTextValueError err)
              Right mid -> Right mid
