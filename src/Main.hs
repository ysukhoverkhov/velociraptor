module Main where

import Control.Monad

main = doStuff

doStuff = do
    putStrLn "Input a line"
    line <- getLine
    putStrLn . rev $ line
    unless(null line) doStuff
    where rev = unwords . reverse . words
