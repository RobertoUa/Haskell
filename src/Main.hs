module Main (main) where
import Data.List
import Data.Char

euler14 = maximum $ map (\x-> (length $ chain x , x)) [1..1000000]    

chain :: (Integral a)=> a -> [a]
chain 1 = [1]
chain n
        | even n = n:chain (div n 2)
        | odd  n = n:chain (n*3 + 1)

{-main = do
  file <- readFile "matrix.txt"
  let matrix = [(read ("[" ++ x ++ "]") :: [Int])|x<-(lines file)]
      maxSum = let reduce xs = zipWith (+) (zipWith min xs (tail xs)) in foldl1 reduce
  print $ maxSum $ transpose matrix-}

main = print euler14