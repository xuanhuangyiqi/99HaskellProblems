-- 54
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [(Branch 'x' l r) | l <- (cbalTree (div (n-1) 2)), r <- (cbalTree (n - 1 - (div (n-1) 2)))] ++ 
                (if (even n) then [Branch 'x' l r | l <- (cbalTree (div n 2)), r <- (cbalTree (div (n-1) 2))] else [])

-- 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = sym l r where
    sym Empty Empty = True
    sym (Branch _ l1 r1) (Branch _ l2 r2) = (sym l1 r2) && (sym l2 r1)
    sym _ _ = False

-- 57
construct :: [Int] -> Tree Int
construct (x:xs) = foldl sch (Branch x Empty Empty) xs where
    sch (Branch x l r) a = 
        if (a > x) then 
            (if r == Empty then 
                Branch x l (Branch a Empty Empty) else 
                Branch x l (sch r a)) else
            (if l == Empty then
                Branch x (Branch a Empty Empty) r else
                Branch x (sch l a) r)
-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
    | even n    = [] 
    | otherwise = [(Branch 'x' l (gen l)) | l <- (cbalTree (div n 2))] where
        gen Empty = Empty
        gen (Branch x l r) = Branch x (gen r) (gen l)

-- 59
hbalTree :: a -> Int -> [Tree a]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x 0 = [Empty]
hbalTree x n = [(Branch x l r) | l <- (hbalTree x (n-2)), r <- (hbalTree x (n-1)) ] ++
                [(Branch x l r) | l <- (hbalTree x (n-1)), r <- (hbalTree x (n-2)) ] ++
                [(Branch x l r) | l <- (hbalTree x (n-1)), r <- (hbalTree x (n-1)) ]

-- 60
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x 0 = [Empty]
hbalTreeNodes x 1 = [Branch x Empty Empty]
hbalTreeNodes x n = [(Branch x l r) | nl <- [0..(n-1)], l <- (hbalTreeNodes x nl), r <- (hbalTreeNodes x (n-1-nl)), (abs ((height l)-(height r))) <= 1] where
    height Empty = 0
    height (Branch x l r) = 1 + (max (height l) (height r))

-- 61
countLeaves :: Tree a -> Int
countLeaves (Branch x Empty Empty) = 1
countLeaves Empty = 0
countLeaves (Branch x l r) = (countLeaves l) + (countLeaves r)

-- 61A
leaves :: Tree a -> [a]
leaves (Branch x Empty Empty) = [x]
leaves Empty = []
leaves (Branch x l r) = (leaves l) ++ (leaves r)

-- 62
internals :: Tree a -> [a]
internals (Branch x Empty Empty) = []
internals Empty = []
internals (Branch x l r) = [x] ++ (internals l) ++ (internals r)

-- 63
atLeavel :: Tree a -> Int -> [a]
atLeavel (Branch x l r) 1 = [x]
atLeavel Empty n = []
atLeavel (Branch x l r) n = (atLeavel l (n-1)) ++ (atLeavel r (n-1))

