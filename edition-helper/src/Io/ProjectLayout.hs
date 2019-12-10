{-|
Module : Model
License : see LICENSE
Description : Make project layout
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Io.ProjectLayout where

import           System.Directory               ( createDirectory )
import           System.FilePath                ( combine
                                                , FilePath(..)
                                                , addExtension
                                                )
import           System.IO
import           Control.Monad                  ( mapM )

--makeDirs :: [String] -> [IO ()]
