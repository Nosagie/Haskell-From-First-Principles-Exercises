module Chapter10Ex where

-- FIX MY MAP AND MYSQUISH

-- MyOr
-- Direct Recursion
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs 

-- Direct Recursion ising ||
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

-- fold, not point free in the fold function
myOr'' [] = False
myOr'' l = foldr (\a b -> if a == True then True else False) False l

-- fold, and &, point free in folding function
myOrF :: [Bool] -> Bool
myOrF = foldr (||) False

-- myAny
-- Direct Recursion
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) == True then True else myAny f xs

--Direct Recursion using ||
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs) = (f x) || (myAny' f xs)

-- fold, not point free
myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f l = foldr (\x y -> (f x)||y) False l

-- fold point free with ||
myAnyF :: (a -> Bool) -> [a] -> Bool
myAnyF  = ((foldr (||) False).).map   

-- MyElem
-- Direct Recursion
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if (e == x) then True else myElem e xs

-- Direct Recursion Using ||
myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' e (x:xs) = (x == e) || myElem' e xs

-- fold, not point free in fold function
myElem'' :: Eq a => a -> [a] -> Bool
myElem'' e l = foldr (\x y -> (||) (e == x) y) False l

-- fold , point free 
myElemF ::Eq a => a -> [a] -> Bool
myElemF = ((foldr (||) False) .).map.(==)

-- using Any
myElemA :: Eq a => a -> [a] -> Bool
myElemA e l = any (==e) l


-- myReverse
-- direct recursion
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- using fold
myReverse' :: [a] -> [a]
myReverse' l = foldl (\x y -> [y]++x) [] l --change to foldl' when you have the chance

-- map in terms of foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr((:).f) []

-- squish direct recursion
squish :: [[a]] -> [a]
squish = foldr (++) [] 


squishH :: [a] -> [a]
squishH [] = []
squishH (x:xs) = [x] ++ squishH xs

-- using fold 
squish' :: [[a]] -> [a]
squish' l = foldr (++) [] l

-- using fold pointfree
squish'' :: [[a]] -> [a]
squish'' = foldr (++) []


 -- squishMap using direct recursion
squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap' _ [] = []
squishMap' f (x:[]) = foldr (\x _ -> f x) [] [x]
squishMap' f (x:xs) = foldr (\x _ -> f x) [] [x] ++ squishMap' f xs  


 -- using fold point free
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


-- squishAgain
squishAgain :: [[a]] -> [a]
squishAgain l = squishMap (\x -> x) l

-- myMaximumBy using fold
myMaximumBy :: (Num a) => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f l = foldl (\x y -> if (f x y == GT) then x else y) 0 l

-- myMinimumBy using fold
myMinimumBy :: (Num a) => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f l = foldl (\x y -> if (f x y == LT) then x else y) 1234567 l







