import Data.Char

maxs :: String -> [Int]
maxs [] = []
maxs xs = (product . take 5 $ map digitToInt xs) : (maxs $ tail xs)

main = do file <- readFile "1000.txt"
          print $ maximum . maxs . concat $ lines file
