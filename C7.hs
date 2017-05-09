module Chapter7 where

f x y z = x * y * z
f' = \x -> \y -> \z -> x * y * z

-- y = \x -> x + 1

addFive = \x y -> (if x > y then y else x) + 5

mflip f x y = f x y 