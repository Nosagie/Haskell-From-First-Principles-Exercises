module Arith3Broken where


main :: IO()
main = do 
        print $ 1 + 2
        printm 10
        print $ negate $ negate 1
        print $ (+) 0 blah
        where blah = negate 1 
