module List where 

myTail :: [a] -> [a]
myTail [] = []
myTail (_ : xs ) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : []) = Nothing
safeTail ( _ :xs ) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

myEnumFromTo :: (Enum a,Ord a) => a -> a -> [a]
myEnumFromTo start stop 
            | start == stop = [stop]
            | start < stop = [start] ++ myEnumFromTo (succ start) stop 
            | otherwise = []


-- Fibs function that returns an infinite list of fib numbers 
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Pointfree style
-- fibsN = (!!) fibs


 