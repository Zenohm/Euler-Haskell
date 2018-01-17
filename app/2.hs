module Main where

import Lib

main = print $ sum $ filter even $ takeWhile (<4000000) $ fibs
