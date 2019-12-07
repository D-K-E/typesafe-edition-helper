{-|
Module : Model
License : see LICENSE
Description : ContainerData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module ContainerData
    ( ContainerData
    )
where
import           Model.Container                ( ContainerModel )
import           Model.Unit                     ( UnitModel )


-- | container data: container data model which can be either a container
-- model or unit model
data ContainerData = NestedCons ContainerModel
                    | SimpleCons UnitModel
                    deriving (Eq, Show)
