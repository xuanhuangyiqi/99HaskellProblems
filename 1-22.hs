-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast (x:xs)
  | length xs == 1  = x
  | otherwise       = myButLast xs

-- 3
element_at :: [a] -> Int -> a
element_at (x:xs) 1 = x
element_at (x:xs) y = element_at xs (y-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == last xs  = isPalindrome (init xs)
  | otherwise     = False

-- 7
data NestedList a = Elem a | List [NestedList a]
my_flatten :: (NestedList a) -> [a]
my_flatten (Elem x) = [x]
my_flatten (List []) = []
my_flatten (List ((Elem x):xs)) = [x] ++ (my_flatten (List xs))
my_flatten (List ((List x):xs)) = my_flatten (List x) ++ (my_flatten (List xs))

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ (compress (filter (/= x) xs))

-- 9
pack :: (Eq a) =>  [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:(filter (== x) xs)):(pack (filter (/= x) xs))

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length (takeWhile (== x) xs) + 1, x):(encode (dropWhile (== x) xs))

-- 11
data Encoding a = Single a | Multiple Int a deriving Show
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified xs = (map conv (encode xs)) where
    conv (1, a) = Single a
    conv (b, a) = Multiple b a

-- 12
decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified ((Single a):xs) = a:(decodeModified xs)
decodeModified ((Multiple num a):xs) = (take num (repeat a))++(decodeModified xs)

-- 13
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect xs = foldr conn [] xs where
    conn x [] = [Single x]
    conn x ((Single a):ys) = if x == a then (Multiple 2 a):ys else ((Single x):(Single a):ys)
    conn x ((Multiple b a):ys) = if x == a then (Multiple (b+1) a):ys else ((Single x):(Multiple b a):ys)

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n (repeat x)) ++ (repli xs n)

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1 where
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) n m = if m `mod` n == 0 then (dropEvery' xs n (m+1)) else (x:(dropEvery' xs n (m+1)))

-- 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n = if n <= 0 then ([], x:xs) else (x:ys, zs) where
    (ys, zs) = split xs (n-1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice (x:xs) s t
    | s > 1 = slice xs (s-1) (t-1)
    | t > 0 = x:(slice xs 0 (t-1))
    | otherwise = []

-- 19
rotate :: [a] -> Int -> [a]
rotate (x:xs) n
    | n == 0    = (x:xs)
    | n > 0     = rotate (xs ++ [x]) (n-1)
    | otherwise = rotate ((last xs):x:(init xs)) (n+1)

-- 20
removeAt :: Int -> [a] -> [a]
removeAt n (x:xs)
    | n == 1    = xs
    | otherwise = x:(removeAt (n-1) xs)

-- 21 
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x:xs
insertAt y (x:xs) n = x:(insertAt y xs (n-1))

-- 22
range :: Int -> Int -> [Int]
range s t
    | s == t    = [s]
    | otherwise = s:(range (s+1) t)

