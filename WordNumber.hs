module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = "Enter digit between 0 and 9"



digits :: Int -> [Int]
digits 0 = []
digits x = digits divres ++ [modres]
        where 
            divres = quot x 10
            modres = mod x 10 
    


wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))




--map (\x -> bool x (-x) (x == 3) )

