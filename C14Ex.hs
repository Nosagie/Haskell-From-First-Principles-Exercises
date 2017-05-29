module C14Ex where

import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonZero)
import Data.List(sort)
import Data.Char
import Test.QuickCheck.Function (apply, Fun(..))

half :: Fractional a => a -> a
half x = x/2

halfIdentity :: Fractional a => a -> a 
halfIdentity = (*2) . half

propHalf :: Float -> Bool
propHalf x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing,True) xs
   where go y (Nothing,t) = (Just y,t)
         go y (Just x, t) = (Just y,x >= y)

propListOrderedInt :: [Int] -> Bool
propListOrderedInt xs = let l = sort xs 
   in  (listOrdered l)  


plusAssociative x y z = x + (y + z) == (x + y) + z

propPlusAssoc :: Int -> Int -> Int -> Bool
propPlusAssoc = plusAssociative 

plusCommutative x y = x + y == y + x

propPlusComm :: Int -> Int -> Bool
propPlusComm = plusCommutative

multAssociative x y z = x * (y * z) == (x * y) * z

propMultAssoc :: Int -> Int -> Int -> Bool
propMultAssoc = multAssociative

multCommutative x y = x * y == y * x 

propMultComm :: Int -> Int -> Bool
propMultComm = multCommutative

propQuotRem :: NonZero Int -> NonZero Int -> Bool
propQuotRem (NonZero x) (NonZero y) = (quot x y)*y + (rem x y) == x

propDivMod :: NonZero Int -> NonZero Int -> Bool
propDivMod (NonZero x) (NonZero y) = (div x y)*y + (mod x y) == x

propExpComm :: Int -> Int -> Bool
propExpComm x y = (x ^ y) == (y ^ x)

propExpAssoc :: Int -> Int -> Int -> Bool
propExpAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z 

propReverse :: [Int] -> Bool 
propReverse xs = (reverse (reverse xs)) == (id xs)

propApply :: Fun Int Int -> Int -> Bool
propApply (Fun _ f) a = (f $ a) == (f a)

propCompose :: Fun Int Double -> Fun Char Int -> Char -> Bool
propCompose (Fun _ f) (Fun _ g) x = (f . g ) x == f(g x)

propFold :: String -> String -> Bool
propFold xs ys = (foldr (:) xs ys) == (xs ++ ys)

propFoldConcat :: [String] -> Bool
propFoldConcat xs = (foldr (++) [] xs) == (concat xs)

propLength :: Int -> String -> Bool
propLength n xs = length (take n xs) == n 

propReadShow :: Int -> Bool
propReadShow x = (read (show x)) == x

square :: Double -> Double
square x = x * x

propSquare :: Double -> Bool
propSquare x = square (sqrt x) == x 

twice f = f . f 
fourTimes = twice . twice 
capitalizeWord :: String -> String
capitalizeWord w
  | null w = w
  | otherwise = [toUpper firstLetter] ++ map toLower others
  where ([firstLetter], others) = splitAt 1 w

propOnceId :: String -> Bool
propOnceId x = (capitalizeWord x) == (twice capitalizeWord x) && (capitalizeWord x) == (fourTimes capitalizeWord x)

propSortId :: [Int] -> Bool
propSortId x = (sort x) == (twice sort x) && (sort x) == (fourTimes sort x)  

data Fool = Fulse | Frue 
        deriving (Eq,Show)

foolGen :: Gen Fool
foolGen = elements [Fulse,Frue]

foolGen' :: Gen Fool
foolGen' = elements [Fulse,Fulse,Frue] 

main :: IO()
main = do
      quickCheck propHalf
      quickCheck propListOrderedInt 
      quickCheck propPlusAssoc
      quickCheck propPlusComm
      quickCheck propMultAssoc
      quickCheck propQuotRem
      quickCheck propDivMod 
      quickCheck propExpComm
      quickCheck propExpAssoc
      quickCheck propReverse
      quickCheck propApply
      quickCheck propCompose
      quickCheck propFold
      quickCheck propFoldConcat
      quickCheck propLength 
      quickCheck propReadShow
      quickCheck propSquare
      quickCheck propOnceId 
      quickCheck propSortId 
