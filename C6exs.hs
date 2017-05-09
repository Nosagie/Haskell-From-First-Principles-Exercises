module C6Exercises where 

data TisAnInteger = TisAn Integer 

instance Eq TisAnInteger where
    TisAn x == TisAn y = x == y  

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = (x == x') && (y == y')

data StringOrInt = TisAnInt Int | TisAString String  

instance Eq StringOrInt where
    TisAnInt x == TisAnInt y = x == y
    TisAString x == TisAString y = x == y
    _ == _ = False

data Pair a = Pair a a 

instance Eq a => Eq (Pair a) where
    Pair x y == Pair x' y' = (x == x') && (y == y')

data Tuple a b = Tuple a b

instance (Eq a,Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _ = False 

data EitherOr a b = Hello a | Goodbye b

instance (Eq a,Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = (x == x')
    (==) (Goodbye x) (Goodbye x') = (x == x')
    (==) _ _ = False

data Mood = Blah | Woot deriving Show

instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq,Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2::Sentence
s2 = Sentence "Julie" "Loves" "dogs" 

data Rocks = Rocks String deriving (Eq,Show)

data Yeah = Yeah Bool deriving (Eq,Show)

data Papu = Papu Rocks Yeah deriving (Eq,Show)






























