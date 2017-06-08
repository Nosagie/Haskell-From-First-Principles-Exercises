module C16Ex where

import Test.QuickCheck
import Test.QuickCheck.Function

--intermission exercises page 741

a = fmap (+1) $ (read "[1]")::[Int]

b = (fmap.fmap) (++ "lol") (Just ["Hi,","Hello"])

c = (*2).(\x -> x - 2)

d = ((return '1' ++).show). (\x -> [x,1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123"++) (fmap show ioi))
    in fmap (*3) changed

--intermission page 747

--1
newtype Identity a = Identity a
   deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary(Identity a) where
  arbitrary = do
      x <- arbitrary
      return (Identity x)

type IntToInt = Fun Int Int
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool 
type IdentityFI = Identity Int -> Bool

--2
data Pair a = Pair a a
    deriving (Eq,Show)

instance Functor Pair where
    fmap f (Pair y z) = Pair (f y) (f z) 

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do 
     x <- arbitrary
     return (Pair x x)

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type PairFI = Pair Int -> Bool

--3
data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where
   fmap f (Two x y) = Two x (f y)

instance (Arbitrary a,Arbitrary b)=> Arbitrary(Two a b) where 
   arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
type TwoFI = Two Int Int -> Bool

--4 
data Three a b c = Three a b c
      deriving (Eq,Show)
instance Functor (Three a b) where 
    fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary x,Arbitrary y, Arbitrary z) => Arbitrary (Three x y z) where 
   arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFI = Three Int Int Int -> Bool

--5
data Three' a b = Three' a b b deriving (Eq,Show)

instance Functor(Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a , Arbitrary b) => Arbitrary(Three' a b) where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        return (Three' x y y)

type Three'FI = Three' Int Int -> Bool
type Three'FC = Three' Int Int -> IntToInt-> IntToInt -> Bool 

--6
data Four a b c d = Four a b c d
     deriving (Eq,Show)
instance Functor(Four a b c) where
    fmap f (Four u v w x) = Four u v w (f x)
instance (Arbitrary a,Arbitrary b,Arbitrary c,Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        u <- arbitrary
        return (Four x y z u)

type FourFI = Four Int Int Int Int -> Bool
type FourFC = Four Int Int Int Int -> IntToInt-> IntToInt -> Bool

--7
data Four' a b = Four' a a a b 
    deriving (Eq,Show)

instance Functor(Four' a) where
    fmap f (Four' w x y z) = (Four' w x y (f z))

instance (Arbitrary a,Arbitrary b) => Arbitrary (Four' a b) where 
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Four' x x x y)

type Four'FI = Four' Int Int -> Bool
type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool

--8
--No, because Trivial is of kind *. types that implement Functor must be of kind * -> *
 


functorCompose' :: (Eq (f c),Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g.f) x) == (fmap g . fmap f $ x)

functorIdentity :: (Functor f,Eq(f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

tests::IO()
tests = do 
       quickCheck (functorCompose' :: IdentityFC)
       quickCheck (functorIdentity :: IdentityFI)   
       quickCheck (functorCompose' :: PairFC)        
       quickCheck (functorIdentity :: PairFI)        
       quickCheck (functorIdentity :: TwoFI)
  
       quickCheck (functorCompose' :: TwoFC)        
       quickCheck (functorCompose' :: ThreeFC)        
       quickCheck (functorIdentity :: ThreeFI)

       quickCheck (functorIdentity::Three'FI)

       quickCheck (functorCompose'::Three'FC)

       quickCheck (functorIdentity :: FourFI)

       quickCheck (functorCompose' :: FourFC)

       quickCheck (functorIdentity :: Four'FI)

       quickCheck (functorCompose' :: Four'FC)


--short exercises
data Possibly a = LolNope | Yeppers a 
     deriving (Eq,Show)

instance Functor Possibly where
     fmap _ LolNope = LolNope
     fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a | Second b 
    deriving (Eq,Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

--it is impossible because of partial application and the kindedness required for the functor typeclass
