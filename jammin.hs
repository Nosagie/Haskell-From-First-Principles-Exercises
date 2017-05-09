module Jammin where
import Data.List

-- JAMMIN EXERCISES
data Fruit = 
        Peach
    |   Plum
    |   Apple
    |   Blackberry
    deriving (Eq,Show,Ord)

data JamJars = Jam {
                fruit::Fruit,
                number :: Int
                }
    deriving (Eq,Show,Ord)

-- cardinality of jamJars is 4 * 256

row1 = Jam Peach 5
row2 = Jam Plum 7
row3 = Jam Apple 9
row4 = Jam Blackberry 7
row5 = Jam Peach 3
row6 = Jam Plum 3
row7 = Jam Apple 10
allJam = [row1,row2,row3,row4,row5,row6,row7]

total :: [JamJars] -> Int
total = sum . map number --pointfree style

mostRow :: [JamJars] -> JamJars
mostRow = foldr (\x y -> if (number x) >= 
                    (number y) then x 
                    else y) 
           (Jam Peach 0) --pointfree

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind

compareKind :: JamJars -> JamJars -> Ordering 
compareKind (Jam k _) (Jam k' _) = compare k k'

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (>) . sortJam  