import Data.Char
import Data.List
import Data.Numbers.Primes
import qualified Data.Set as Set
import qualified Data.Map as Map

qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

data BookInfo = Book Integer String [String]
                deriving (Show)

list = [321,31,12,43,64,324,1,23,4,5,6,7,8]

fib n = fibs !! n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

euler1 = sum [x | x <- [1..1000], x `mod` 3 == 0 || x `mod` 5 == 0]
euler2 = sum . takeWhile(<4000000) $ filter odd fibs
euler3 = last $ primeFactors 600851475143
euler4 = maximum [z|x<-[100..999], y<-[x..999], let z = x*y ,isPalindrome . show $ z]
euler5 = foldl1 lcm [1..20]
euler6 = sum [1..100] ^2 - sum (map (^2) [1..100])
euler7 = primes !! 10000
--euler8 = let file <- readFile "1000.txt" in maximum . maxs . concat $ lines file --do file <- readFile "1000.txt"
euler9 = head [a*b*c | c<- [100..], b<-[100..c], a<-[100..b], a^2 + b^2 == c^2, a+b+c == 1000]
euler10 = sum (takeWhile (<2000000) primes)
euler14 = maximum $ map (\x->(length $ chain x , x)) [1..1000000]	
euler16 = sum . map digitToInt . show $ 2^1000
euler20 = sum . map digitToInt . show $ product [1..100]
euler48 = sum [x^x | x <- [1..1000]] `mod` 10^10


lastButOne (x:_:[]) = x 
lastButOne (x:y:xs) = lastButOne xs

find' :: Int -> [a] -> a
find' 1 (x:_) = x
find' n (_:xs) = find' (n-1) xs

myLength = foldl1 (flip $ const(+1))

myReverse = foldl (\x y->y: x) []

compress []		= []
compress (x:xs) = x: comp' xs x
	where
		comp' [] _ = []
		comp' (x:xs) priv = if x == priv
							then comp' xs x
							else x : comp' xs x


chain :: (Integral a)=> a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (div n 2)
	| odd  n = n:chain (n*3 + 1)


isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

primes' = 2 : filter ((==1) . length . primeFactors') [3,5..]
 
primeFactors' n = factor n primes'
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps


triList = [truncate $ x*(x-1)/2|x<-[1..]]
eul12 = find (\n-> 5000 < length [div n x | x<-[1..n],mod n x == 0]) triList

maxs :: String -> [Int]
maxs [] = []
maxs xs = (product . take 5 $ map digitToInt xs) : (maxs $ tail xs)