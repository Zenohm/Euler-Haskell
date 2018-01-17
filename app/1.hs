module Main where

import Lib

mults = \ n -> mod n 3 == 0 || mod n 5 == 0
main = print $ sum $ takeWhile (<1000) $ filter mults [1..]
