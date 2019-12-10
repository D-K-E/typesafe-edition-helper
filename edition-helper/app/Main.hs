{-|
Module : Model
License : see LICENSE
Description : Main entry point to program
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Main where

import           Lib
import           System.Directory               ( listDirectory )
import           System.FilePath                ( combine
                                                , FilePath(..)
                                                )
import           Control.Monad                  ( mapM )

showDirContent :: [FilePath] -> IO [()]
showDirContent contents = do
    mapM putStrLn contents

main :: IO ()
main = do
    putStrLn "Enter project directory: "
    dataDirPath <- getLine
    contents    <- listDirectory dataDirPath
    putStrLn "Here are the contents of the directory: "
    showDirContent contents
    putStrLn "Choose an action from the list below: "
    putStrLn "  - Add a new field (a)"
    putStrLn "  - Validate document structure (v)"
    putStrLn "  - Label document part (l)"
    putStrLn "  - Translate document part (t)"
    putStrLn "  - Make ids for document part (m)"
    putStrLn "  - Replace ids (r)"
    choice <- getLine
    putStrLn "Done"
