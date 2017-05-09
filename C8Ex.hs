sumTo :: (Eq a, Num a) => a -> a
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

multi :: (Integral a) => a -> a -> a
multi x y = if y == 0 then 0 else  x + multi x (y-1)


data DividedResult = Result Integer | DividedByZero

dividedBy :: Integer -> Integer -> (DividedResult,Maybe Integer)
dividedBy n d = go n d 0
    where go n d count
            | d == 0 = (DividedByZero,Nothing)
            | n < d = (Result count,Just n)
            | otherwise = go (n-d) d (count + 1)


mc91 :: (Ord a, Num a) => a -> a
mc91 n 
    | n > 100 = n - 10
    | otherwise = mc91. mc91 $ n + 11