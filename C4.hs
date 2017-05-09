module Chapter4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah 


greetIfCold :: String -> IO()
greetIfCold coolness = 
    if coolF coolness then 
        putStrLn "What's up homie"
    else
        putStrLn "Meh."

coolF :: String -> Bool
coolF x = "cold as f" == x 

greet :: String -> IO()
greet x = putStrLn $ "Hello " ++ x


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x 

myAbs :: Integer -> Integer
myAbs x = if x == 0 then 0 else x*(-1)

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ((snd x,snd y),(fst x,fst y))

x = (+)

f' :: String -> Int
f' xs = w `x` 1
        where w = length xs

g x = fst x

i = \x -> x 
s = \(x:xs) -> x
 