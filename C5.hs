funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"


nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337


typCurriedFunc :: Integer -> Bool -> Integer
typCurriedFunc i b = (+) i $ nonsense b 

anon :: Integer -> Bool -> Integer
anon = \i b -> (+) i $ nonsense b 

anonMan :: Integer -> Bool -> Integer
anonMan = \i -> \b -> (+) i $ nonsense b 


g :: a -> b -> c -> b 
g x y z = y

h :: (Num a,Num b) => a -> b -> b 
h  x y = y

jackal :: (Ord a,Eq b) => a -> b -> a 
jackal x y = x

kessel :: (Ord a,Num b) => a -> b -> a
kessel x y = x

p :: a -> a -> a 
p = undefined

s :: a -> a -> a 
s x y  = x

f' x y = x + y + 3