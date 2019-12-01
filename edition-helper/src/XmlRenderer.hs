module XmlRenderer where

-- render container and unit as xml
import           Unit                           ( UnitModel )
import           Container                      ( ContainerModel )
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module
import           Text.Hamlet.XML
import           Text.XML


-- utils functions

add2Map :: Map -> [(k, v)] -> Map

add2Map aMap kvs | kvs == [] = aMap
                 | otherwise = Dict.union aMap (Dict.fromList kvs)

-- transform unit model to xml

add2UnitProps :: String -> String -> UnitModel -> Map
add2UnitProps idName typeName um =
    add2Map (unitProps um) [(idName, (unitId um)), (typeName, (unitType))]


-- transform container model to xml
add2ContainerProps :: String -> String -> ContainerModel -> Map
add2UnitProps idName typeName um = add2Map
    (containerProps um)
    [(idName, (containerId um)), (typeName, (containerType))]
