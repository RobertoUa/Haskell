module Main (main) where

import Control.Monad
import Data.Array.IO
{-euler67 = do
  file <- readFile "file.txt"
  print $ map ((map (\x -> read x :: Integer)).words) $ lines file      -}


main :: IO ()       
main = do
  arr <- (newArray (1,28123) True) :: IO (IOArray Int Bool)
  mapM_ (\n -> writeArray arr n False) [x + y|x <- list, y <- list, x + y <= 28123]       
  print =<< (liftM sum $ filterM (liftM (==True) . readArray arr) [1..28123])
    where list = filter abund [12..28123]
            where abund n = (sum $ [x|x<-[1..n `div` 2], n `mod` x == 0]) > n
