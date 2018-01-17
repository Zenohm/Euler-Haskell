module Lib
    ( factors,
      fib,
      fibs,
      isqrt,
      isDivisible,
      isPalindrome,
      isPrime,
      palindromes,
      primeFactors,
      products
    ) where

import Data.List
import Data.Ord

factors :: Int -> [Int]
factors n = takeWhile  (< isqrt n) $ filter (isDivisible n) [1..]

fib n = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isqrt = floor . sqrt . fromIntegral

isDivisible :: (Num a, Eq a, Integral a) => a -> a -> Bool
isDivisible a b = mod a b == 0

isPalindrome :: Integer -> Bool
isPalindrome x = let w = show x in w == reverse w

isPrime 1 = False
isPrime k = null [ x | x <- [2..isqrt k], k `mod`x  == 0]

palindromes ns = filter isPalindrome ns

primeFactors = (filter isPrime) . factors

products :: [Integer] -> [Integer] -> [Integer]
products ns ms = [x * y | x <- ns, y <- ms]

squares = map (^ 2) [1..]

nSquares = flip take squares

nSquareSum :: Integer -> Integer
nSquareSum n = sum $ nSquares n

nSumSquare :: Int -> Integer
nSumSquare n = (^2) $ sum $ take n [1..]

squareSumDifference :: Integer -> Integer
squareSumDifference n = nSumSquare n - nSquareSum n
