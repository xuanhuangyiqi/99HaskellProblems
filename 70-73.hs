data Tree a = Node a [Tree a] deriving (Eq, Show)

-- 70C
nnodes :: Tree a -> Int
nnodes (Node a []) = 1
nnodes (Node a ts) = 1 + (foldl (\x y-> x + (nnodes y)) 0 ts)

-- 70
count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) = if x == y then 1+(count x ys) else (count x ys)


tl :: [Char] -> [Int]
tl xs = [ x | x <- [0..length(xs)], x == 2*(count '^' (take x xs))]

stringToTree (x:xs) = Node x (subToTree $ init xs) where
    toSubString :: [Int] -> [Char] -> [[Char]]
    toSubString [x] ys = []
    toSubString (x:xs) ys = (take ((head xs)-x+1) (drop x ys)):(toSubString xs ys)
    subToTree [] = []
    subToTree xxs = [stringToTree x | x <- (toSubString (tl xxs) xxs)]

-- 71
ipl :: Tree x -> Int
ipl (Node x []) = 0
ipl (Node x xs) = (length xs) + (foldl (\x y->x+(ipl y)) 0 xs)

-- 72
bottom_up :: Tree Char -> [Char]
bottom_up (Node x ys) = (foldl (\x y->x++(bottom_up y)) "" ys) ++ [x]

