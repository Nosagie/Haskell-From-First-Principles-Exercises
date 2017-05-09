import Data.Char

filtUpper :: String -> String
filtUpper s = [x | x <- s, isUpper x]

capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = [toUpper x] ++ xs 

capAll :: String -> String
capAll (x:[]) = [toUpper x]
capAll (x:xs) = [toUpper x] ++ capAll xs

capHead :: String -> Char 
capHead [] = ' '
capHead (x:_) = toUpper x
