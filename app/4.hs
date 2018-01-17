module Main where

import Lib

main = print $ maximum $ palindromes $ products [100..999] [100..999]
