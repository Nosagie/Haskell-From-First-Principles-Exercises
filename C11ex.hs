{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11Ex where

data Price = Price Integer deriving (Eq,Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq,Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq,Show)


data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq,Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 89000)


isCar :: Vehicle -> Bool
isCar (Car _ _) = True 
isCar _  = False


isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True 
isPlane _ = False


areCars :: [Vehicle] -> [Bool]
areCars l = map isCar l

getManu :: Vehicle -> Manufacturer 
getManu (Car m _) = m 
getManu _ = error "Only cars have manufacturers" 

-- instance classes
-- data Goats = Goats Int deriving (Eq,Show)

newtype Goats = Goats Int deriving (Eq,Show,TooMany)
newtype Cows = Cows Int deriving (Eq,Show)

newtype IntStringTuple = IntStringTuple (Int,String) deriving (Eq,Show) 

newtype SumTuple = SumTuple (Int,Int)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance  TooMany Int where
      tooMany n = n  > 42

instance  TooMany IntStringTuple where
    tooMany (IntStringTuple (n,s)) = n > 42

instance  TooMany SumTuple where
    tooMany (SumTuple (x,y))= tooMany (x+y) 

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x,y) = tooMany $ x + y 
-- instance TooMany Goats where
--      tooMany (Goats n) = tooMany n 

-- Records 
data Person1 = MkPerson String Int deriving (Eq,Show)

-- using record syntax
data Person =
    Person {name::String 
        ,age :: Int }
        deriving (Eq,Show)

jm = Person "julie" 18
ca = Person "chris" 16

namae :: Person1 -> String
namae (MkPerson s _) = s 

data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
            deriving Show 



