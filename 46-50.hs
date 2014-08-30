-- 46
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True, False], y <- [True, False]]



-- 48
rep :: Int -> [[Bool]]
rep 1 = [[True], [False]]
rep n = [x:xs | x <- [True, False], xs <- (rep (n-1))]

toStr :: [Bool] -> [Char]
toStr [] = ""
toStr (x:xs) = (show x) ++ " " ++ (toStr xs)

tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f = mapM_ putStrLn [(toStr x) ++ show (f x) | x <- rep n]

-- 49
