module Main (main) where
import Data.List
import Data.Char
{-euler67 = do
  file <- readFile "file.txt"
  print $ map ((map (\x -> read x :: Integer)).words) $ lines file      
-}
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

main :: IO ()       
main = print $ filterAll $ sortBy (flip compare) $ permutations ['1'..'7']
  where filterAll = find (\str -> isPrime (read str :: Integer)) 