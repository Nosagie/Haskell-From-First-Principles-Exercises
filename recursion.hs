module Chapter8Recursion where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1) 


inc :: Num a => a -> a
inc = (+1)

three :: Num a => a
three = inc . inc . inc $ 0

three' :: Num a => a
three' = (inc.inc.inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n 
incTimes times n = 1 + incTimes (times - 1)  n 

applyTimes :: (Eq a,Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a 
incTimes' times n = applyTimes times (+1) n 

f :: Bool -> Int 
f True = error "blah"
f False = 0

f' :: Bool -> Maybe Int
f' False = Just 0
f' _ = Nothing 

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = x + fibonacci(x-1)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer


