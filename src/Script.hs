import Data.Char
import Data.List
import Data.Ratio
import Data.Array.IO
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

euler1 = sum [x | x <- [1..1000], x `mod` 3 == 0 || x `mod` 5 == 0]
euler2 = sum . takeWhile(<4000000) $ filter odd fibs
euler3 = last $ primeFactors 600851475143
euler4 = maximum [z|x<-[100..999], y<-[x..999], let z = x*y ,isPalind . show $ z]
euler5 = foldl1 lcm [1..20]
euler6 = sum [1..100] ^2 - sum (map (^2) [1..100])
euler7 = primes !! 10000
euler8 = do file <- readFile "1000.txt" 
            print $ maximum . maxs . concat $ lines file
euler9 = head [a*b*c | c<- [100..], b<-[100..c], a<-[100..b], a^2 + b^2 == c^2, a+b+c == 1000]
euler10 = sum (takeWhile (<2000000) primes)
euler12 = find (\n-> 500 < divisors n) $ scanl1 (+) [1..]
  where divisors n = product $ map ((1+) . length) (group $ primeFactors n)
euler13 = do
  file <- readFile "file.txt"
  print $ take 10 $ show $ foldl (\acc nums -> (read nums :: Integer) + acc) 0 $ lines file
euler14 = maximum $ map (\x-> (length $ chain x , x)) [1..1000000]        
euler16 = sum . map digitToInt . show $ 2^1000
euler20 = sum . map digitToInt . show $ product [1..100]
euler21 = sum $ filter amicable [1..10000]
  where amicable n = divis num == n && n /= num
          where num = divis n
                divis n = sum $ [x|x<-[1..n `div` 2], n `mod` x == 0]
euler22 = do
  names <- readFile "names.txt"
  print $ foldl (\acc tp -> fst tp * (digSum tp) + acc) 0 $ makeList names
    where digSum n   = sum $ map (\x-> ord x - 64) $ snd n
          makeList  :: Integral a => String -> [(a, String)]
          makeList n = zip [1..] (sort . read $ "[" ++ n ++ "]")    
euler23 = do
  arr <- (newArray (1,28123) True) :: IO (IOArray Int Bool)
  mapM_ (\n -> writeArray arr n False) [x + y|x <- list, y <- list, x + y <= 28123]       
  print =<< (liftM sum $ filterM (liftM (==True) . readArray arr) [1..28123])
    where list = filter abund [12..28123]
            where abund n = (sum $ [x|x<-[1..n `div` 2], n `mod` x == 0]) > n
euler24 = (sort $ permutations "0123456789") !! 999999
euler25 = head $ dropWhile ((<1000) . length . show . fib) [1..]
euler27 = maximum $ [(length $ takeWhile isPrime [n^2 + a*n + b | 
          n <- [0..]], a*b) | a <- [-999..999], b <- [-999..999]]
euler28 = print $ (spiral 1001, spira2 1001)
  where spira2 n = sum . scanl (+) 1 . concat . map (replicate 4 . (*2)) $ [1..n `div` 2]
        spiral n  = spiral' 1 2 (n*n) 1
          where spiral' t m n s
                   | summ > n  = 1
                   | t == 4    = summ + spiral' 1 (m+2)  n  summ
                   | otherwise = summ + spiral' (t+1) m  n  summ
                       where summ = s + m                       
euler29 = length $ nub [a^b | a<-[2..100], b<-[2..100]]
euler30 = sum $ filter isSumOfFifth [1000..500000]
  where isSumOfFifth n = n == (foldr ((+) . (^5) . digitToInt) 0 $ show n)
euler31 = 1+ sum [1|one<-[0..200], two<-0:[2,4..200], five<-0:[5,10..200], ten<-0:[10,20..200],
                    twenty<-0:[20,40..200], fifty<-0:[50,100..200], hundred<-[0,100,200],
                    one+two+five+ten+twenty+fifty+hundred==200]
euler32 = sum $ nub [a*b | a<-[1..50], b<-[1..2000], (a*b)^2>10^6, isPandig a b]
  where isPandig a b = concatMap show [1..9] == sort combined
                  where combined = show a ++ show b ++ show (a*b)
euler34 = sum $ filter curious [10..facs!!9]
  where curious n = n == (foldr ((+) . (facs!!) . digitToInt) 0 $ show n) 
euler35 = length $ filter (all isPrime . rotations) [1..1000000]
  where rotations n = rotations' 0 (show n)
          where rotations' n st
                  | n==(length st) = []
                  | otherwise      = (read cool :: Integer) : rotations' (n+1) cool
                      where cool   = (last st : init st)
euler36 = sum $ filter isBothPalind [1..1000000]
  where isBothPalind n = isPalind (convRadix 2 n) && (isPalind $ show n)
euler37 = sum $ filter (isAllPrime . show) [10..1000000]
  where isAllPrime n = all isPrime . toDigit $ init $ tail $ inits n ++ tails n
          where toDigit strList = map (\x -> read x :: Integer) strList
euler39 = maximum $ map (\n-> (length n, head n)) $ group $ sort solutions 
  where solutions = [a+b+c|b<-[1..400], a<-[1..b],let c = cc a b, a^2+b^2 == c^2, a+b+c<=1000]
            where cc a b = round $ sqrt $ (fromInteger a)^2+(fromInteger b)^2
euler40 = product $ map digitToInt $ [concatMap show [1..] !! (10^n-1)|n<-[1..6]]
euler41 = filterAll $ sortBy (flip compare) $ permutations ['1'..'7']
  where filterAll = find (\str -> isPrime (read str :: Integer))
euler45 = floor $ head $ filter isPent [n*(2*n-1)|n<-[145..]]
  where isPent n = sqr == fromInteger (round sqr)
          where sqr = (sqrt (24*n+1)+1) / 6
euler47 = find (\x -> (length . nub $ collect x) == 16) [1..]
  where collect x = concatMap (group . primeFactors) [x..x+3]                     
euler48 = sum [x^x | x <- [1..1000]] `mod` 10^10
euler49 = head $ concat [seqn num | num <- [1000..], isPrime num]
  where seqn num = [(x,y,z)|let cool = perms num, x<-cool,y<-cool,z<-cool,z-y==y-x,x<y, y<z,x>1487]
          where perms num = filter (isPrime) $ map (\x->read x :: Integer) $ permutations $ show num
euler52 = head $ filter (\x-> all (==True) [isTheSame x (x*n)|n<-[2..6]]) [1..]
  where isTheSame n n2 = listOfDigits n == listOfDigits n2
          where listOfDigits num = sort $ map digitToInt $ show num
euler53 = length $ filter (>10^6) [binomCoeff n r | n<-[1..100], r<-[1..n]]
euler56 = print $ maximum [sum . map digitToInt . show $ a^b | a<-[50..100], b<-[50..100]]
euler57 = length $ filter (\x->lng numerator x > lng denominator x) [1..1000]
              where lng f x = length $ show $ f $ fraction $ 1:replicate x 2
euler79 = do  file <- readFile "keylog.txt"
              let list   = lines file
                  answer = find (\xs->(length $ filter (\y-> intersect xs y /= y) list) == 0) perms
                    where perms = permutations $ sort $ nub $ concat list
              print answer
euler97 = reverse $ take 10 $ reverse $ show $ 28433*2^7830457+1

facs = scanl (*) 1 [1..]
fib n = fibs!!n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

binomCoeff n k = facs !! n `div` (facs !! (n-k) * facs !! k)

chain :: (Integral a)=> a -> [a]
chain 1 = [1]
chain n
        | even n = n:chain (div n 2)
        | odd  n = n:chain (n*3 + 1)


isPalind []  = True
isPalind [_] = True
isPalind xs  = (head xs) == (last xs) && (isPalind $ init $ tail xs)



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

fraction [x] = x%1
fraction (x:xs) = x%1 + 1/(fraction xs)



twoex = zip ns ds 
    where
    ns = 3 : zipWith (\x y -> x + 2 * y) ns ds
    ds = 2 : zipWith (+) ns ds

convRadix :: (Integral b) => b -> b -> [b]
convRadix n = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` n, b `div` n))


