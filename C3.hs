module Chapter3 where

main :: IO()
main = putStrLn "Hello World!"

main1 :: IO()
main1 = do 
    putStrLn "Count one to four for me"
    putStr " One! "
    putStr " Two!"
    putStrLn " Three!"
    putStrLn " and, Four!"

-- For main3 module
myGreeting :: String
myGreeting = "Hello " ++ "World!"

hello :: String
hello = "Hello"

world :: String
world = "World!"

main3 :: IO()
main3 = do 
    putStrLn myGreeting
    putStrLn secondGreeting
        where secondGreeting = concat[hello," ",world]

main4 :: IO()
main4 = do
    putStrLn greet 
        where greet = (++) hello $ (++) " " world

-- Exercises
ex1 :: String -> String
ex1 x = x ++ "!"

ex2 :: String -> Char 
ex2 x = x !! 4

ex3 :: String -> String
ex3 x = drop 9 x

ex4 :: String -> Char
ex4 x = x !! 2

ex5 :: Int -> Char
ex5 x = "Curry" !! x

rvrs :: String
rvrs =   (drop 9 "Curry is awesome") ++ drop 5 (take 9 "Curry is awesome") ++ take 5 "Curry is awesome" 

