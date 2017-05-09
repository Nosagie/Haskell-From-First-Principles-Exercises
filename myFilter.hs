myFilter :: String -> [String]
myFilter s = [x | x <- words s, notElem x ["the","a","an"]] 


--Zip allows you to combine values in a list - returns list of tuples
--stops when one of lists runs out of values
--Unzip may lose information due to this

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _  = []
myZip _ []  = []
myZip (x:xs) (y:ys)  = [(x,y)] ++ myZip xs ys 

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = [(f x y)] ++ myZipWith f xs ys 


myZip' :: [a] -> [b] -> [(a,b)]
myZip' x y = myZipWith (\x y -> (x,y)) x y