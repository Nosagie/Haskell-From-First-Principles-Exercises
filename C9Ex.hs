module C9Exercises where


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x:xs) = if f x then True else False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) 
                | y == x = True
                | otherwise = myElem y xs  

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


squish :: [[a]] -> [a]
squish (x:[]) = squishH x ++ []
squish (x:xs) = squishH x ++ squish xs


-- Squish helper function
squishH :: [a] -> [a] 
squishH [] = []
squishH (x:xs) = [x] ++ squishH xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [[]] = []
squishAgain (x:[]) = squishMap squish [[x]]
squishAgain (x:xs) = (squishMap squish [[x]]) ++ squishAgain xs


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Exception: Empty List"
myMaximumBy f (x:xs) = myMaximumByHelper f x xs
-- Helper function for myMaximumBy
myMaximumByHelper :: (a -> a -> Ordering) -> a -> [a] -> a
myMaximumByHelper _ h [] = h 
myMaximumByHelper f h (x:xs)
                | (f h x) == GT = myMaximumByHelper f h xs
                | otherwise = myMaximumByHelper f x xs  


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy _ [] = error "Exception: Empty List"
myMinimumBy f (x:xs) = myMinimumByHelper f x xs
-- Helper function for myMinimumBy
myMinimumByHelper :: (a -> a -> Ordering) -> a -> [a] -> a 
myMinimumByHelper _ h [] = h 
myMinimumByHelper f h (x:xs)
                | (f h x) == LT = myMinimumByHelper f h xs
                | otherwise = myMinimumByHelper f x xs


myMaximum :: (Ord a) => [a] -> a 
myMaximum [] = error "Empty List"
myMaximum x = myMaximumBy (\x y -> compare x y) x


myMinimum :: (Ord a) => [a] -> a 
myMinimum [] = error "Empty List"
myMinimum x = myMinimumBy (\x y -> compare x y) x









    