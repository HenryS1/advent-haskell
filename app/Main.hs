module Main where

import Lib
import Day15

main :: IO ()
main = part2 >>= (putStrLn . show)
