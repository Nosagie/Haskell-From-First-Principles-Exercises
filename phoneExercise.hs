module PhoneExercise where

import Data.List
import Data.Char 

data DaPhone =  DaPhone [(Digit,Presses)]

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do you think I am prety Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

-- validButtons "1234567890*#"
type Digit = Char

-- valid presses = [1..4]
type Presses = Int

-- there is probably a more elegant way to do this, but it works
cellPhonesDead :: DaPhone -> String -> [(Digit,Presses)]
cellPhonesDead _ []  = []
cellPhonesDead p (x:xs) 
        | x == 'A' || x == 'B' || x == 'C' || x == '2' = 
            case x of 'A' -> [('*',1),('2',1)] ++ cellPhonesDead p xs
                      'B' -> [('*',1),('2',2)] ++ cellPhonesDead p xs
                      'C' -> [('*',1),('2',3)] ++ cellPhonesDead p xs
                      '2' -> [('2',4)] ++ cellPhonesDead p xs
        | x == 'D' || x == 'E' || x == 'F' || x == '3' = 
            case x of 'D' -> [('*',1),('3',1)] ++ cellPhonesDead p xs
                      'E' -> [('*',1),('3',2)] ++ cellPhonesDead p xs
                      'F' -> [('*',1),('3',3)] ++ cellPhonesDead p xs
                      '3' -> [('3',4)] ++ cellPhonesDead p xs
        | x == 'G' || x == 'H' || x == 'I' || x == '4' = 
            case x of 'G' -> [('*',1),('4',1)] ++ cellPhonesDead p xs
                      'H' -> [('*',1),('4',2)] ++ cellPhonesDead p xs
                      'I' -> [('*',1),('4',3)] ++ cellPhonesDead p xs
                      '4' -> [('4',4)] ++ cellPhonesDead p xs
        | x == 'J' || x == 'K' || x == 'L' || x == '5' = 
            case x of 'J' -> [('*',1),('5',1)] ++ cellPhonesDead p xs
                      'K' -> [('*',1),('5',2)] ++ cellPhonesDead p xs
                      'L' -> [('*',1),('5',3)] ++ cellPhonesDead p xs
                      '5' -> [('5',4)] ++ cellPhonesDead p xs
        | x == 'M' || x == 'N' || x == 'O' || x == '6' = 
            case x of 'M' -> [('*',1),('6',1)] ++ cellPhonesDead p xs
                      'N' -> [('*',1),('6',2)] ++ cellPhonesDead p xs
                      'O' -> [('*',1),('6',3)] ++ cellPhonesDead p xs
                      '6' -> [('6',4)] ++ cellPhonesDead p xs
        | x == 'P' || x == 'Q' || x == 'R' || x == 'S' || x == '7' = 
            case x of 'D' -> [('*',1),('7',1)] ++ cellPhonesDead p xs
                      'E' -> [('*',1),('7',2)] ++ cellPhonesDead p xs
                      'F' -> [('*',1),('7',3)] ++ cellPhonesDead p xs
                      'S' -> [('*',1),('7',4)] ++ cellPhonesDead p xs
                      '7' -> [('7',5)] ++ cellPhonesDead p xs
        | x == 'T' || x == 'U' || x == 'V' || x == '8' = 
            case x of 'T' -> [('*',1),('8',1)] ++ cellPhonesDead p xs
                      'U' -> [('*',1),('8',2)] ++ cellPhonesDead p xs
                      'V' -> [('*',1),('8',3)] ++ cellPhonesDead p xs
                      '8' -> [('8',4)] ++ cellPhonesDead p xs
        | x == 'W' || x == 'X' || x == 'Y' || x == 'Z' || x == '9' = 
            case x of 'W' -> [('*',1),('9',1)] ++ cellPhonesDead p xs
                      'X' -> [('*',1),('9',2)] ++ cellPhonesDead p xs
                      'Y' -> [('*',1),('9',3)] ++ cellPhonesDead p xs
                      'Z' -> [('*',1),('9',4)] ++ cellPhonesDead p xs
                      '9' -> [('9',5)] ++ cellPhonesDead p xs
        | x == 'a' || x == 'b' || x == 'c' = 
            case x of 'a' -> [('2',1)] ++ cellPhonesDead p xs
                      'b' -> [('2',2)] ++ cellPhonesDead p xs
                      'c' -> [('2',3)] ++ cellPhonesDead p xs
        | x == 'd' || x == 'e' || x == 'f'  = 
            case x of 'd' -> [('3',1)] ++ cellPhonesDead p xs
                      'e' -> [('3',2)] ++ cellPhonesDead p xs
                      'f' -> [('3',3)] ++ cellPhonesDead p xs
        | x == 'g' || x == 'h' || x == 'i' = 
            case x of 'g' -> [('4',1)] ++ cellPhonesDead p xs
                      'h' -> [('4',2)] ++ cellPhonesDead p xs
                      'i' -> [('4',3)] ++ cellPhonesDead p xs
        | x == 'j' || x == 'k' || x == 'l' = 
            case x of 'j' -> [('5',1)] ++ cellPhonesDead p xs
                      'k' -> [('5',2)] ++ cellPhonesDead p xs
                      'l' -> [('5',3)] ++ cellPhonesDead p xs
        | x == 'm' || x == 'n' || x == 'o'  = 
            case x of 'm' -> [('6',1)] ++ cellPhonesDead p xs
                      'n' -> [('6',2)] ++ cellPhonesDead p xs
                      'o' -> [('6',3)] ++ cellPhonesDead p xs
        | x == 'p' || x == 'q' || x == 'r' || x == 's'  = 
            case x of 'p' -> [('7',1)] ++ cellPhonesDead p xs
                      'q' -> [('7',2)] ++ cellPhonesDead p xs
                      'r' -> [('7',3)] ++ cellPhonesDead p xs
                      's' -> [('7',4)] ++ cellPhonesDead p xs
        | x == 't' || x == 'u' || x == 'v'  = 
            case x of 't' -> [('8',1)] ++ cellPhonesDead p xs
                      'u' -> [('8',2)] ++ cellPhonesDead p xs
                      'v' -> [('8',3)] ++ cellPhonesDead p xs
        | x == 'w' || x == 'x' || x == 'y' || x == 'z' = 
            case x of 'w' -> [('9',1)] ++ cellPhonesDead p xs
                      'x' -> [('9',2)] ++ cellPhonesDead p xs
                      'y' -> [('9',3)] ++ cellPhonesDead p xs
                      'z' -> [('9',4)] ++ cellPhonesDead p xs
        | x == '^' || x == '*' =
            case x of '^' ->  [('*',1)] ++ cellPhonesDead p xs
                      '*' ->  [('^',2)] ++ cellPhonesDead p xs
        | x == '+' || x == '_' || x == '0' || x == ' ' =
            case x of '+' ->  [('+',1)] ++ cellPhonesDead p xs
                      '_' ->  [('_',2)] ++ cellPhonesDead p xs
                      '0' ->  [('0',3)] ++ cellPhonesDead p xs
                      ' ' ->  [('0',4)] ++ cellPhonesDead p xs
        | x == '#' || x == '.' || x == ',' =
            case x of '.' ->  [('.',1)] ++ cellPhonesDead p xs
                      ',' ->  [(',',2)] ++ cellPhonesDead p xs
                      '#' ->  [('#',3)] ++ cellPhonesDead p xs
        | otherwise = []


-- map (cellPhonesDead . DaPhone $ [(' ',3)]) convo
-- map fingerTaps ((map (cellPhonesDead . DaPhone $ [(' ',3)]) convo))


t = (DaPhone [(' ',0)])

fingerTaps :: [(Digit,Presses)] -> Presses
fingerTaps l = foldr (\(v,z) b -> z + b) 0 l 

reverseTaps :: Char -> [(Digit,Presses)]
reverseTaps x = cellPhonesDead t [x]

popularest :: String -> Char
popularest = fst .foldr 
            (\(c,l) (d, l2)-> 
                (if l > l2 then (c,l) else (d,l2))) 
            (' ',0)
            .map (\x -> if (toLower . head $ x) == ' ' then (' ',0) else (toLower . head $ x,length x))
            . group . sort . map toLower

coolestLtr :: [String] -> Char
coolestLtr = popularest . map popularest 

-- a lot of stuff going on, but it works :D
coolestWord :: [String] -> String
coolestWord =  fst.foldr 
                (\(a,b) (c,d) -> 
                    if b > d then (a,b) else (c,d)) (" ",0)
               .map (\x -> (head x,length x))
               .group.sort.words.foldr 
               (\x y -> x ++ " " ++ y) " "
               .map (\(x:xs) -> x:xs) 


-- Not in exercise, playing around
-- zips to get numtaps and digit, then foldr to get max, then returns char
mostCostly :: String -> Char
mostCostly " " = ' '
mostCostly s = fst.foldr 
            (\(x,y) (a,b) -> 
                if y > b then (x,y) else (a,b))
            (' ',0)
            . zipWith getLetter
            (map reverseTaps s) $ s

-- helper function for zip in most costly word
getLetter :: [(Digit,Int)] -> Char -> (Char,Int)
getLetter _ ' ' = (' ',0)
getLetter [] c = (c,0) 
getLetter [(x,y)] c =  (c,y) 
getLetter [(x,y),(z,v)] c =  (c,y+v) 



