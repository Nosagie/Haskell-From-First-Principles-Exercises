module C7Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d 
    where xLast = (div) x 10
          d     = (mod) xLast 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd $ divMod (fst $ divMod x 10) 10  

hunsD ::Integral a => a -> a 
hunsD x = snd $ divMod d2 10 
    where 
        d2 = fst $ divMod x 100 

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
        | z == True = x 
        | otherwise = y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y z = if z == True then x else y

g :: (a -> b) -> (a,c) -> (b,c)
g f (x,y) = (f x,y) 

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do 
    print $ roundTrip 4
    print $ id 4

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b --add type to argument to make work
roundTrip'' = read . show