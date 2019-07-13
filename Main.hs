import Data.List (group)

-- Problem 01
myLast :: [a] -> a
myLast = foldr1 (const id)

-- Problem 02
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

-- Problem 03
elementAt :: [a] -> Int -> a
elementAt xs k = head . drop (k - 1) $ xs

-- Problem 04
myLength :: [a] -> Int
myLength = sum . map (const 1)

-- Problem 05
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

-- Problem 07
data NestedList a
    = Elem a
    | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 08
compress :: (Eq a) => [a] -> [a]
compress xs = map head (group xs)

-- Problem 09
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

-- Problem 11
data Encoded a
    = Single a
    | Multiple Int a
    deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map f . encode
  where
    f (1,x) = Single x
    f (n,x) = Multiple n x

-- Problem 12
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap f
-- alternative: concatMap (uncurry replicate)
  where
    f (Single x) = [x]
    f (Multiple n x) = replicate n x

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = Single x : encodeDirect xs
  | otherwise = Multiple count x : encodeDirect rest
  where
    (matched,rest) = span (== x) xs

    count = 1 + length matched

-- Problem 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs n = go xs n
  where
    go [] _ = []
    go all @ (x:xs) cur
      | cur > 0 = x : go all (cur - 1)
      | otherwise = go xs n

-- TODO: Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = undefined

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs n k = take (1 + k - n) $ drop (n - 1) xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotate xs (n + len)
  | n > len = rotate xs (n - len)
  | otherwise =
      let (left,right) = splitAt n xs
      in right ++ left
  where
    len = length xs

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k = go (k - 1) []
  where
    go 0 left (x:right) = (x, left ++ right)
    go n left (x:right) = go (n - 1) (left ++ [x]) right
