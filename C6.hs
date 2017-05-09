module ShowTest where

data Mood = Blah

instance Show Mood where
    show _ = "Blah"
 
data Trivial = Trivial'|NonTrivial'

instance Eq Trivial where
    Trivial' == Trivial' = True
    NonTrivial' == NonTrivial' = True
    _ == _ = False

data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun 
    deriving (Show)

data Date = 
    Date DayOfWeek Int 

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

-- Make Friday the best day
instance Ord DayOfWeek where
    compare Fri Fri = EQ 
    compare Fri _ = GT 
    compare _ Fri = LT 
    compare _ _ = EQ 

instance  Eq Date where
    (==) (Date weekday monthNum) (Date weekday' monthNum') = 
        weekday == weekday' && monthNum == monthNum'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v)(Identity v') = v == v'


