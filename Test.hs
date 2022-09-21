pow :: Int -> Int -> Int
pow 1 n = 1
pow x 1 = x
pow x n
        | even n = let y = pow x (n `div` 2) in y*y
        | otherwise = x * pow x (n-1)

binSearch :: Integral a => (a -> Bool) -> a -> a -> a
binSearch p l u 
            | u - l == 1    = u --lower and upper are next to eachother
            | p h           = binSearch p l h  
            | otherwise     = binSearch p h u
        where h = (u+l) `div` 2 

is5 :: Int -> Bool
is5 n 
    | n>4       = True
    | otherwise = False
    
apxSqrt :: Double -> Double -> Double
apxSqrt eps x = guess 1
    where
    guess n = let y = 0.5*(n+x/n) in 
        if closeEnough n then y else guess y 
        where closeEnough a = abs(a * a - x) < eps

