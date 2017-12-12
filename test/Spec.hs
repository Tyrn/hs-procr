module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [ "app/Main.hs"
               , "src/Lib.hs"
               ]
