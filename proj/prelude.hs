:reload
reload
data Bool
= False
| True
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool
not :: Bool -> Bool
otherwise :: Bool
data Maybe a
= Nothing
| Just a
maybe :: b -> (a -> b) -> Maybe a -> b
data Either a b
= Left a
| Right b
either :: (a -> c) -> (b -> c) -> Either a b -> c
data Ordering
= LT
| EQ
| GT
data Char
type String = [Char]
fst :: (a, b) -> a
snd :: (a, b) -> b
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
class Eq a => Ord a where
compare :: a -> a -> Ordering
(<) :: a -> a -> Bool
(>=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(<=) :: a -> a -> Bool
max :: a -> a -> a
min :: a -> a -> a
class Enum a where
succ :: a -> a
pred :: a -> a
toEnum :: Int -> a
fromEnum :: a -> Int
enumFrom :: a -> [a]
enumFromThen :: a -> a -> [a]
enumFromTo :: a -> a -> [a]
enumFromThenTo :: a -> a -> a -> [a]
class Bounded a where
minBound :: a
maxBound :: a
data Int
data Integer
data Float
data Double
type Rational = Ratio Integer
class Num a where
(+) :: a -> a -> a
(*) :: a -> a -> a
(-) :: a -> a -> a
negate :: a -> a
abs :: a -> a
signum :: a -> a
fromInteger :: Integer -> a
class (Num a, Ord a) => Real a where
toRational :: a -> Rational
class (Real a, Enum a) => Integral a where
quot :: a -> a -> a
rem :: a -> a -> a
div :: a -> a -> a
mod :: a -> a -> a
quotRem :: a -> a -> (a, a)
divMod :: a -> a -> (a, a)
toInteger :: a -> Integer
class Num a => Fractional a where
(/) :: a -> a -> a
recip :: a -> a
fromRational :: Rational -> a
class Fractional a => Floating a where
pi :: a
exp :: a -> a
sqrt :: a -> a
log :: a -> a
(**) :: a -> a -> a
logBase :: a -> a -> a
sin :: a -> a
tan :: a -> a
cos :: a -> a
asin :: a -> a
atan :: a -> a
acos :: a -> a
sinh :: a -> a
tanh :: a -> a
cosh :: a -> a
asinh :: a -> a
atanh :: a -> a
acosh :: a -> a
class (Real a, Fractional a) => RealFrac a where
properFraction :: Integral b => a -> (b, a)
truncate :: Integral b => a -> b
round :: Integral b => a -> b
ceiling :: Integral b => a -> b
floor :: Integral b => a -> b
class (RealFrac a, Floating a) => RealFloat a where
floatRadix :: a -> Integer
floatDigits :: a -> Int
floatRange :: a -> (Int, Int)
decodeFloat :: a -> (Integer, Int)
encodeFloat :: Integer -> Int -> a
exponent :: a -> Int
significand :: a -> a
scaleFloat :: Int -> a -> a
isNaN :: a -> Bool
isInfinite :: a -> Bool
isDenormalized :: a -> Bool
isNegativeZero :: a -> Bool
isIEEE :: a -> Bool
atan2 :: a -> a -> a
subtract :: Num a => a -> a -> a
even :: Integral a => a -> Bool
odd :: Integral a => a -> Bool
gcd :: Integral a => a -> a -> a
lcm :: Integral a => a -> a -> a
(^) :: (Num a, Integral b) => a -> b -> a
(^^) :: (Fractional a, Integral b) => a -> b -> a
fromIntegral :: (Integral a, Num b) => a -> b
realToFrac :: (Real a, Fractional b) => a -> b
class Monad m where
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
return :: a -> m a
fail :: String -> m a
class Functor f where
fmap :: (a -> b) -> f a -> f b
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()
(=<<) :: Monad m => (a -> m b) -> m a -> m b
id :: a -> a
const :: a -> b -> a
(.) :: (b -> c) -> (a -> b) -> a -> c
flip :: (a -> b -> c) -> b -> a -> c
($) :: (a -> b) -> a -> b
until :: (a -> Bool) -> (a -> a) -> a -> a
asTypeOf :: a -> a -> a
error :: [Char] -> a
undefined :: a
seq :: a -> b -> b
($!) :: (a -> b) -> a -> b
map :: (a -> b) -> [a] -> [b]
(++) :: [a] -> [a] -> [a]
filter :: (a -> Bool) -> [a] -> [a]
head :: [a] -> a
last :: [a] -> a
tail :: [a] -> [a]
init :: [a] -> [a]
null :: [a] -> Bool
length :: [a] -> Int
(!!) :: [a] -> Int -> a
reverse :: [a] -> [a]
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl1 :: (a -> a -> a) -> [a] -> a
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr1 :: (a -> a -> a) -> [a] -> a
and :: [Bool] -> Bool
or :: [Bool] -> Bool
any :: (a -> Bool) -> [a] -> Bool
all :: (a -> Bool) -> [a] -> Bool
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
concat :: [[a]] -> [a]
concatMap :: (a -> [b]) -> [a] -> [b]
maximum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
iterate :: (a -> a) -> a -> [a]
repeat :: a -> [a]
replicate :: Int -> a -> [a]
cycle :: [a] -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
break :: (a -> Bool) -> [a] -> ([a], [a])
elem :: Eq a => a -> [a] -> Bool
notElem :: Eq a => a -> [a] -> Bool
lookup :: Eq a => a -> [(a, b)] -> Maybe b
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
lines :: String -> [String]
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String
type ShowS = String -> String
class Show a where
showsPrec :: Int -> a -> ShowS
show :: a -> String
showList :: [a] -> ShowS
shows :: Show a => a -> ShowS
showChar :: Char -> ShowS
showString :: String -> ShowS
showParen :: Bool -> ShowS -> ShowS
type ReadS a = String -> [(a, String)]
class Read a where
readsPrec :: Int -> ReadS a
readList :: ReadS [a]
reads :: Read a => ReadS a
readParen :: Bool -> ReadS a -> ReadS a
read :: Read a => String -> a
lex :: ReadS String
data IO a
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
getChar :: IO Char
getLine :: IO String
getContents :: IO String
interact :: (String -> String) -> IO ()
type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()
readIO :: Read a => String -> IO a
readLn :: Read a => IO a
type IOError = IOException
ioError :: IOError -> IO a
userError :: String -> IOError
catch :: IO a -> (IOError -> IO a) -> IO a

Data.List

{-
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
-}

Data.List



(++) :: [a] -> [a] -> [a]
head :: [a] -> a
last :: [a] -> a
tail :: [a] -> [a]
init :: [a] -> [a]
null :: [a] -> Bool
length :: [a] -> Int
map :: (a -> b) -> [a] -> [b]
reverse :: [a] -> [a]
intersperse :: a -> [a] -> [a]
intercalate :: [a] -> [[a]] -> [a]
transpose :: [[a]] -> [[a]]
subsequences :: [a] -> [[a]]
permutations :: [a] -> [[a]]
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1' :: (a -> a -> a) -> [a] -> a
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr1 :: (a -> a -> a) -> [a] -> a
concat :: [[a]] -> [a]
concatMap :: (a -> [b]) -> [a] -> [b]
and :: [Bool] -> Bool
or :: [Bool] -> Bool
any :: (a -> Bool) -> [a] -> Bool
all :: (a -> Bool) -> [a] -> Bool
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
maximum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
iterate :: (a -> a) -> a -> [a]
repeat :: a -> [a]
replicate :: Int -> a -> [a]
cycle :: [a] -> [a]
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
break :: (a -> Bool) -> [a] -> ([a], [a])
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
group :: Eq a => [a] -> [[a]]
inits :: [a] -> [[a]]
tails :: [a] -> [[a]]
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf :: Eq a => [a] -> [a] -> Bool
elem :: Eq a => a -> [a] -> Bool
notElem :: Eq a => a -> [a] -> Bool
lookup :: Eq a => a -> [(a, b)] -> Maybe b
find :: (a -> Bool) -> [a] -> Maybe a
filter :: (a -> Bool) -> [a] -> [a]
partition :: (a -> Bool) -> [a] -> ([a], [a])
(!!) :: [a] -> Int -> a
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndices :: Eq a => a -> [a] -> [Int]
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndices :: (a -> Bool) -> [a] -> [Int]
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
lines :: String -> [String]
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String
nub :: Eq a => [a] -> [a]
delete :: Eq a => a -> [a] -> [a]
(\\) :: Eq a => [a] -> [a] -> [a]
union :: Eq a => [a] -> [a] -> [a]
intersect :: Eq a => [a] -> [a] -> [a]
sort :: Ord a => [a] -> [a]
insert :: Ord a => a -> [a] -> [a]
nubBy :: (a -> a -> Bool) -> [a] -> [a]
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
maximumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy :: (a -> a -> Ordering) -> [a] -> a
genericLength :: Num i => [b] -> i
genericTake :: Integral i => i -> [a] -> [a]
genericDrop :: Integral i => i -> [a] -> [a]
genericSplitAt :: Integral i => i -> [b] -> ([b], [b])
genericIndex :: Integral a => [b] -> a -> b
genericReplicate :: Integral i => i -> a -> [a]


Data.Char

{-
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
-}

Data.Char

data Char
type String = [Char]
isControl :: Char -> Bool
isSpace :: Char -> Bool
isLower :: Char -> Bool
isUpper :: Char -> Bool
isAlpha :: Char -> Bool
isAlphaNum :: Char -> Bool
isPrint :: Char -> Bool
isDigit :: Char -> Bool
isOctDigit :: Char -> Bool
isHexDigit :: Char -> Bool
isLetter :: Char -> Bool
isMark :: Char -> Bool
isNumber :: Char -> Bool
isPunctuation :: Char -> Bool
isSymbol :: Char -> Bool
isSeparator :: Char -> Bool
isAscii :: Char -> Bool
isLatin1 :: Char -> Bool
isAsciiUpper :: Char -> Bool
isAsciiLower :: Char -> Bool
data GeneralCategory
= UppercaseLetter
| LowercaseLetter
| TitlecaseLetter
| ModifierLetter
| OtherLetter
| NonSpacingMark
| SpacingCombiningMark
| EnclosingMark
| DecimalNumber
| LetterNumber
| OtherNumber
| ConnectorPunctuation
| DashPunctuation
| OpenPunctuation
| ClosePunctuation
| InitialQuote
| FinalQuote
| OtherPunctuation
| MathSymbol
| CurrencySymbol
| ModifierSymbol
| OtherSymbol
| Space
| LineSeparator
| ParagraphSeparator
| Control
| Format
| Surrogate
| PrivateUse
| NotAssigned
generalCategory :: Char -> GeneralCategory
toUpper :: Char -> Char
toLower :: Char -> Char
toTitle :: Char -> Char
digitToInt :: Char -> Int
intToDigit :: Int -> Char
ord :: Char -> Int
chr :: Int -> Char
showLitChar :: Char -> ShowS
lexLitChar :: ReadS String
readLitChar :: ReadS Char

{-
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
-}
(>>=) :: forall a b. m a -> (a -> m b) -> m b
(>>) :: forall a b. m a -> m b -> m b
return :: a -> m a
fail :: String -> m a
class Monad m => MonadPlus m where
mzero :: m a
mplus :: m a -> m a -> m a
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
forever :: Monad m => m a -> m b
void :: Functor f => f a -> f ()
join :: Monad m => m (m a) -> m a
msum :: MonadPlus m => [m a] -> m a
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapAndUnzipM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
replicateM :: Monad m => Int -> m a -> m [a]
replicateM_ :: Monad m => Int -> m a -> m ()
guard :: MonadPlus m => Bool -> m ()
when :: Monad m => Bool -> m () -> m ()
unless :: Monad m => Bool -> m () -> m ()
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
ap :: Monad m => m (a -> b) -> m a -> m b

mempty :: a
mappend :: a -> a -> a
mconcat :: [a] -> a
(<>) :: Monoid m => m -> m -> m


pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a
empty :: f a
(<|>) :: f a -> f a -> f a
some :: f a -> f [a]
many :: f a -> f [a]
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$) :: Functor f => a -> f b -> f a
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
optional :: Alternative f => f a -> f (Maybe a)