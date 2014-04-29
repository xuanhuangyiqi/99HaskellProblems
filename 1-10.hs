myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:xs)
  | length xs == 1  = x
  | otherwise       = myButLast xs

element_at :: [a] -> Int -> a
element_at (x:xs) 1 = x
element_at (x:xs) y = element_at xs (y-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == last xs  = isPalindrome (init xs)
  | otherwise     = False

data NestedList a = Elem a | List [NestedList a]
my_flatten :: (NestedList a) -> [a]
my_flatten (Elem x) = [x]
my_flatten (List []) = []
my_flatten (List ((Elem x):xs)) = [x] ++ (my_flatten (List xs))
my_flatten (List ((List x):xs)) = my_flatten (List x) ++ (my_flatten (List xs))

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ (compress (filter (/= x) xs))

pack :: (Eq a) =>  [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:(filter (== x) xs)):(pack (filter (/= x) xs))

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length (takeWhile (== x) xs) + 1, x):(encode (dropWhile (== x) xs))
