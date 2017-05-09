module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myWords :: String -> [String]
myWords [] = []
myWords x = 
    let 
        without_word_one = dropWhile (/= ' ') x
        new_word = dropWhile (== ' ') $ without_word_one
    in
        [takeWhile (/= ' ') x ] ++ myWords new_word

myLines :: String -> [String]
myLines [] = []
myLines x = [line] ++ myLines restOfString
    where
        line = takeWhile (/= '\n') x 
        restOfStringWithNewLine = dropWhile (==' ') $ dropWhile (/= '\n') x 
        restOfString = dropWhile (=='\n') restOfStringWithNewLine


-- mySplit :: String -> Char -> [String]


-- myWords' :: String -> [String]
-- myWords' [] = []
-- myWords' x = [head (mySplit x ' ')]  ++ myWords' ((mySplit x ' ') !! 1)


shouldEqual = [ "Tyger Tyger, burning bright"
                , "In the forests of the night"
                , "What immortal hand or eye"
                , "Could frame thy fearful symmetry?" ]

main :: IO()
main = 
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)   




rev' :: [a] -> [a]
rev' [] = []
rev' [x] = [x]
rev' (x:xs) = (rev' xs) ++ [x]


rev'' :: [[a]] -> [[a]]
rev'' [[]] = [rev' []]
rev'' [[x]] = [rev' [x]]
rev'' ((x:xs):ys) = rev'' ys ++ [(rev' (x:xs))]






