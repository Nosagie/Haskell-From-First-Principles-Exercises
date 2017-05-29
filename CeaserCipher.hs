module CeaserCipher where

import Data.Char
import Test.QuickCheck

-- Implements ceaser cipher
ceaser :: String -> Int -> String
ceaser (x:[]) s = [shiftLetter x s] 
ceaser (x:xs) s =  [shiftLetter x s] ++ ceaser xs s 

-- Unceasers string
unCeaser :: String -> Int -> String
unCeaser (x:[]) s = [unShiftLetter x s]
unCeaser (x:xs) s = [unShiftLetter x s] ++ unCeaser xs s

-- shifts letter for ceaser cipher, buggy when wrapping words
shiftLetter :: Char -> Int -> Char
shiftLetter c s 
			| newIndex >= 97 && newIndex <= 122 = chr newIndex
			| newIndex >= 65 && newIndex <= 90  = chr newIndex
			| newIndex >= 122 = chr $ 97 + mod newIndex 122
			| newIndex >= 90 = chr $ 65 + mod newIndex 90
			| otherwise = ' '
		where newIndex = ord c + s

-- unshifts letter
unShiftLetter :: Char -> Int -> Char 
unShiftLetter ' ' _ = ' '
unShiftLetter c s 
			|  newIndex > 97 && newIndex < 122 = chr newIndex
			|  newIndex >= 65 && newIndex < 90  = chr newIndex
			|  newIndex < 65 = chr $ 90 - mod (65 - newIndex) 65
			|  newIndex < 97 = chr $ 122 - mod (97 - newIndex) 97
			|  otherwise = ' '
		where newIndex = ord c - s;

propCeaser :: String -> Int -> Bool
propCeaser s n = (ceaser s n) == (unCeaser (ceaser s n) n)

tests :: IO()
tests = quickCheck propCeaser

main :: IO()
main = do 
	putStr "Message to encrypt: "
	message <- getLine 
	putStr "Num of shifts: "
	shifts <- getLine
	putStrLn . (++) "Result: " . ceaser message $ (read shifts::Int) 
