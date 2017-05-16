module AsPatterns where

import Data.Char

-- using fold and map
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf x y = foldr (&&) True
            . map (\z -> elem z y) $ x

-- using as patterns
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = True
isSubsequenceOf' a@(x:xs) b@(y:ys) = elem x b  
            &&  isSubsequenceOf' xs b


capitalizeWords :: String -> [(String,String)]
capitalizeWords [] = []
capitalizeWords w =  helper . words $ w
            where 
                helper :: [String] -> [(String,String)]
                helper [] = []
                helper (b@(y:ys):xs) = [(b,[toUpper y] ++ ys)] 
                                ++ helper xs


-- language exercises
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
        | x == ' ' = capitalizeWord xs 
        | otherwise = [toUpper x] ++ xs 

-- splits string by given char
splitBy :: String -> Char -> [String]
splitBy s c =  map capitalizeWord.lines . 
                map (\z -> if z == c then '\n' else z) 
                $ s

-- capitalizes paragraph in word
capitalizeParagraph :: String -> String
capitalizeParagraph w = unwords 
                . map capitalizeWord 
                . splitBy w $ '.'