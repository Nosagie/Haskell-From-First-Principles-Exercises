module VignereCipher where
import Data.Char


-- 1 weekday su a type with five data constructors

-- 2 f :: Weekday -> String 

-- 3 types defined with data keyword must begin with capital letter

-- 4 delivers the final element of xs 



vignere :: [Char] -> [Char] -> [Char]
vignere s k = e . zipWith (\x y -> (x,mod (ord y) 65)) s $ zipWordKey s (cycle k)
        

e :: [(Char,Int)] -> [Char]
e [] = []
e ((x,y):xs) = [charShift x y] ++ e xs

zipWordKey :: [Char] -> [Char] -> [Char]
zipWordKey [] k = []
zipWordKey w@(x:xs) k@(y:ys)
            | x == ' ' = " " ++ zipWordKey xs k 
            | otherwise = [toUpper y] ++ zipWordKey xs ys   

charShift :: Char -> Int -> Char
charShift ' ' _ = ' '
charShift char shift = 
            chr . (+) 65 
            . mod (mod ((ord char) + shift) 65) 
            $ 26

main :: IO()
main = do 
    putStr "message: "
    msg <- getLine 
    putStr "keyword: "
    key <- getLine
    putStrLn . (++) "Result: " . vignere msg $ key 