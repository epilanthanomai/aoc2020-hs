module Main where

import Paths_aoc2020 (getDataFileName)

main :: IO ()
main = getDataFileName "samples/hello.txt" >>= readFile >>= putStr
