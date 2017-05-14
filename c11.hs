module C11 where

data Product a b = a :&: b
                deriving (Eq,Show)