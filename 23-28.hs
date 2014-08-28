-- 23
import System.Random
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return (take n [xs !! x | x <- randomRs (0, (length xs) - 1) gen])

-- 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
    gen <- getStdGen
    return (take n (randomRs (1, m) gen))

-- 25
rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu (x:xs) = do
    ind <- randomRIO (0, (length xs) - 2)
    left <- rnd_permu xs
    return ((take ind left) ++ [x] ++ (drop ind left))

-- 26
rm :: Eq a => [a] -> a -> [a]
rm [] _ = []
rm (x:xs) y
    | x == y    = xs
    | otherwise = x:(rm xs y)

combinations :: Eq a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = [(y:ys) | y <- xs, ys <- (combinations (n-1) (rm xs y))]

-- 27
comb :: Int -> [a] -> [[a]]
comb 1 xs = [[x] | x <- xs]
comb n xs 
    | (length xs) == n  = [xs]
    | otherwise         = [(head xs):ys | ys <- comb (n-1) (tail xs)] ++ (comb n (tail xs))

lrm :: Eq a => [a] -> [a] -> [a]
lrm xs [] = xs
lrm (x:xs) al@(y:ys)
    | x == y    = lrm xs ys
    | otherwise = x:(lrm xs al)

group :: Eq a => [Int] -> [a] -> [[[a]]]
group (n:ns) xs
    | ns == []  = [comb n xs]
    | otherwise = [y:ys | y <- (comb n xs), ys <- (group ns (lrm xs y)) ]

-- 28
miner :: [a] -> [a] -> Bool
miner x y = ((length x) < (length y))

fq :: [[a]] -> Int -> Int
fq [] _ = 0 
fq (x:xs) y
    | length(x) == y    = 1+(fq xs y)
    | otherwise         = fq xs y

fminer :: [[a]] -> [a] -> [a] -> Bool
fminer s x y = (fq s (length x)) < (fq s (length y))

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = (lsort [y | y <- xs, miner y x]) ++ [x] ++ (lsort [y | y <- xs, not (miner y x)])

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort a@(x:xs) = (lfsort [y | y <- xs, fminer a y x]) ++ [x] ++ (lfsort [y | y <- xs, not (fminer a y x)]) 
