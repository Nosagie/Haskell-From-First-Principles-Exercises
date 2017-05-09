module Learn where

sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")


printInc n = print (plusTwo + w)
    where plusTwo = n + 2
          w = 4

printInc' n = let plusTwo' = n + 2
    in print plusTwo'

myResult x = x * 5


a = 7
b = 10
f x y= x + y

