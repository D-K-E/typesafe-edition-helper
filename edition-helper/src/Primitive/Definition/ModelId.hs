{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.Definition.ModelId
    ( ModelId(..)
    )
where
import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text ) -- importing type

-- | model id: alphanumeric non empty string has to be unique for each model
data ModelId = TextIdCons Text
                | StringIdCons String
                deriving (Eq, Show)
