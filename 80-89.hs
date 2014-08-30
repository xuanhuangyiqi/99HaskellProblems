data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adj a = Adj [(a, [a])] deriving (Show, Eq)

-- 80
graphToAdj :: Eq a => (Graph a) -> (Adj a)
graphToAdj (Graph v e) = Adj [(x, (f x e)) | x <- v] where
    f node e = [fst ee | ee <- e, snd ee == node] ++ [snd ee | ee <- e, fst ee == node]

-- 81
paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths s t e
    | s == t    = [[t]]
    | otherwise = [s:p | ee <- e, fst ee == s, p <- (paths (snd ee) t e)]

-- 82
pcycle :: Eq a => a -> [(a, a)] -> [[a]]
pcycle a es = [(a:s) | x <- es, s <- (paths (snd x) a es), (fst x) == a]

