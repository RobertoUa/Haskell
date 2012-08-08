module Main (main) where
import Data.Char
import Data.Ratio
import Data.List
euler47 = find (\x -> (length . nub $ coll x) == 4*4) [1..]
	where coll x = concatMap (group . primeFactors) [x..x+3]
		 

primes = sieve [2..]
  where
    sieve (x:xs) = x : sieve [n | n <- xs, rem n x > 0]
primes' = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

euler12 = find (\n-> 500 < divisors n) triangleNumbers
  where divisors n = product $ map ((1+) . length) (group $ primeFactors n)
triangleNumbers = scanl1 (+) [1..]


 
e = 2 : concat [ [1, 2*i, 1] | i <- [1..] ]
 
fraction [x] = x%1
fraction (x:xs) = x%1 + 1/(fraction xs)
 
problem_65 = sum $ map digitToInt $ show $ numerator $ fraction $ take 100 e
main :: IO ()                
main = print euler12