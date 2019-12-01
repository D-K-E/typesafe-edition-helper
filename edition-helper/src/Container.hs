module Container
    ( ContainerModel
    , ContainerData
    )
where
-- container module for containing units

import           Unit
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module



-- Model Declaration

data ContainerData = NestedContainer [ContainerData]
                    | SimpleContainer UnitModel
                     deriving (Show, Eq)

data ContainerProperty = CProp {
     key :: String
    ,val :: String
    } deriving (Show, Eq)


data ContainerModel = ContainerModel {
     containerId :: String
    ,containerType :: String
    ,containerProps :: Map
    ,containerData :: ContainerData
    } deriving (Show, Eq)

-- End of Model Declaration
