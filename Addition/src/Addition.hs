module Main where 

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integer -> Integer -> (Integer,Integer)
dividedBy n d = go n d 0
    where go n d count
            | d == 0 = (0,0)
            | n < d = (count,n)
            | otherwise = go (n-d) d (count + 1)

multi :: (Integral a) => a -> a -> a
multi x y = if y == 0 then 0 else  x + multi x (y-1)

main :: IO()
main = hspec $ do 
    describe "Addition" $ do
       it "15 divided by 3 is 5" $ do 
           dividedBy 15 3 `shouldBe` (5,0) 
       it "22 divided bhy 5 is 4 rem 2" $ do
           dividedBy 22 5 `shouldBe` (4,2)
       it "2 multiplied by 12 is 24" $ do 
           multi 2 12 `shouldBe` 24 
       it "0 multiplied by 56 is 0" $ do
           multi 0 56 `shouldBe` 0
       it "x + 1 is always greater than x" $ do 
           property $ \x -> x + 1 > (x::Int)
