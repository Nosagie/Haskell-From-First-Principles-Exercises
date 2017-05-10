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

type AuthorName = String 

data Author = Author (AuthorName, BookType)

-- data Author =
--     Fiction AuthorName
--     | NonFiction AuthorName
--     deriving (Eq,Show)
-- in Normal Form

data Expr = 
    Number Int 
    | Add Expr Expr 
    | Minus Expr 
    | Mult Expr Expr 
    | Divide Expr Expr

-- Exercise 
data FlowerType = Gardenia 
                | Daisy
                | Rose 
                | Lilac
                deriving Show

type Gardener = String 

data Garden = Garden Gardener FlowerType
            deriving Show

-- data Garden = 
--          Gardener Gardenia
--         |Gardener Daisy
--         |Gardener Rose
--         |Gardener Lilac
--         deriving Show   
-- In Normal form 

data GuessWhat = Chickenbutt deriving (Eq,Show)

data Id a = MkId a deriving (Eq,Show)

data Product a b = Product a b deriving (Eq,Show)

data Sum a b = 
     First a
    |Second b 
    deriving (Eq,Show)

data RecordProduct a b = 
    RecordProduct { pfirst :: a,
                    psecond :: b}
                    deriving (Eq,Show)

newtype NumCow = NumCow Int 
            deriving (Eq,Show)

newtype NumPig = NumPig Int 
            deriving (Eq,Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq,Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int 
        deriving (Eq,Show)

type BigFarmhouse' = 
    Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool 

type PoundsOfWool = Int 

data CowInfo = CowInfo Name Age 
            deriving (Eq,Show)

data PigInfo = PigInfo Name Age LovesMud
            deriving (Eq,Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
        deriving (Eq,Show)


data Animal = 
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq,Show)

-- Or
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer 
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

type Awesome = Bool
-- type Name = String 

person :: Product Name Awesome 
person = Product "Simon" True 

data Twitter = Twitter deriving (Eq,Show)

data AskFm = AskFm deriving (Eq,Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

myRecord :: RecordProduct Integer Float 
myRecord = RecordProduct {pfirst = 42, 
                psecond = 0.00001}



















