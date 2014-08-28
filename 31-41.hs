-- 31
isPrime :: Int -> Bool
isPrime n = length [x | x<-[2..n], n `mod` x == 0] == 1

-- 32
myGCD :: Int -> Int -> Int
myGCD x 0 = abs x
myGCD x y = myGCD y (x `mod` y)

-- 33
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

-- 35
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = let pr = \x n -> if (mod n x == 0) then x else (pr (x+1) n) in ((pr 2 n):(primeFactors (div n (pr 2 n)))) 

-- 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = 
    foldr (\a b -> if ((not (b == [])) && ((fst (head b)) == a))
                    then ((fst (head b)), ((snd (head b))+1)):(tail b) 
                    else ((a,1):b)) 
        [] 
        (primeFactors n)

-- 39
primeR :: Int -> Int -> [Int]
primeR x y = filter (\x -> length [y | y <- [2..(x-1)], (mod x y) == 0] == 0) [x..y]

-- 40
goldbach :: Int -> (Int, Int)
goldbach n = foldr (\y x -> if (isPrime y) && (isPrime (n-y)) then (y, n-y) else x) (0, n) [2..(n-1)]

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList m n = [goldbach x | x <- [m..n], mod x 2 == 0]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' m n k = [x | x <- goldbachList m n, fst x > k, snd x > k]
