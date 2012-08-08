import Data.Char
import Data.List
import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map



euler1 = sum [x | x <- [1..1000], x `mod` 3 == 0 || x `mod` 5 == 0]
euler2 = sum . takeWhile(<4000000) $ filter odd fibs
euler3 = last $ primeFactors 600851475143
euler4 = maximum [z|x<-[100..999], y<-[x..999], let z = x*y ,isPalindrome . show $ z]
euler5 = foldl1 lcm [1..20]
euler6 = sum [1..100] ^2 - sum (map (^2) [1..100])
euler7 = primes !! 10000
--euler8 = do file <- readFile "1000.txt" print $ maximum . maxs . concat $ lines file
euler9 = head [a*b*c | c<- [100..], b<-[100..c], a<-[100..b], a^2 + b^2 == c^2, a+b+c == 1000]
euler10 = sum (takeWhile (<2000000) primes)
euler12 = find (\n-> 500 < divisors n) scanl1 (+) [1..]
  where divisors n = product $ map ((1+) . length) (group $ primeFactors n)
euler14 = maximum $ map (\x-> (length $ chain x , x)) [1..1000000]        
euler16 = sum . map digitToInt . show $ 2^1000
euler20 = sum . map digitToInt . show $ product [1..100]
euler25 = head $ dropWhile ((<1000) . length . show . fib) [1..]
euler31 = 1+ sum [1|one<-[0..200], two<-0:[2,4..200], five<-0:[5,10..200], ten<-0:[10,20..200],
                    twenty<-0:[20,40..200], fifty<-0:[50,100..200], hundred<-[0,100,200],
                    one+two+five+ten+twenty+fifty+hundred==200] 
euler47 = find (\x -> (length . nub $ collect x) == 16) [1..]
  where collect x = concatMap (group . primeFactors) [x..x+3]                     
euler48 = sum [x^x | x <- [1..1000]] `mod` 10^10


facs = scanl (*) 1 [1..]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  
primes2 = sieve [2..]
  where
    sieve (x:xs) = x : sieve [n | n <- xs, rem n x > 0]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors2 n = factor n primes2
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

binomCoeff n k = facs !! n `div` (facs!!(n-k) * facs!!k)


chain :: (Integral a)=> a -> [a]
chain 1 = [1]
chain n
        | even n = n:chain (div n 2)
        | odd  n = n:chain (n*3 + 1)


isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)



maxs :: String -> [Int]
maxs [] = []
maxs xs = (product . take 5 $ map digitToInt xs) : (maxs $ tail xs)

qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

solveRPN :: String -> Float  
solveRPN = head . foldl evaluate [] . words  
    where  evaluate (x:y:ys) "*" = (x * y):ys  
           evaluate (x:y:ys) "+" = (x + y):ys  
           evaluate (x:y:ys) "-" = (y - x):ys  
           evaluate (x:y:ys) "/" = (y / x):ys
           evaluate  xs number   = read number:xs  









