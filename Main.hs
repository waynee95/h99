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
myReverse = foldl (\acc val -> val : acc) []

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
