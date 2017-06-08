{-# LANGUAGE FlexibleInstances #-}

module C16EX2 where 


import Test.QuickCheck
import Test.QuickCheck.Function


--chapter exercises for Chapter 16

--1 No
--2 Yes
--3 Yes
--4 Yes
--5 No

--next section
--1 data Sum b a = First a | Second b
--2 data Company a c b= DeepBlue a c | Something b
--3 data More b a = L a b a | R b a b



--next section
--1
data Quant a b = Finance | Desk a | Bloor b
    deriving (Eq,Show)

instance Functor(Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        elements [Finance,Desk x,Bloor y]

--2 similar to const Functor
data K a b = K a
     deriving (Eq,Show)

instance Functor (K a) where
    fmap _ (K x) = K x  

instance (Arbitrary a,Arbitrary b) => Arbitrary (K a b) where
    arbitrary = do 
        x <- arbitrary 
        return (K x ) 

--3 
newtype Flip f a b = Flip (f b a)
    deriving (Eq,Show)

instance Functor (Flip K a) where
    fmap f (Flip (K x)) = Flip (K (f(x)))

--4
data EvilGoateeConst a b = GoatyConst b
    deriving (Eq,Show)

instance Functor(EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)  

instance (Arbitrary a,Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = do 
        y <- arbitrary
        return (GoatyConst y) 

--5
data LiftItOut f a = LiftItOut (f a)
    deriving (Eq,Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6
data Parappa f g a = DaWrappa (f a) (g a) 

instance (Functor f,Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)  

--7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f,Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa fb) = IgnoringSomething fa (fmap f fb)

--8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f  gt) 
 

--9
data List a = Nil | Cons a (List a)
    deriving (Eq,Show)
instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons x y) = Cons (f x) (fmap f y) 

--10
data GoatLord a = NoGoat | OneGoat a 
        | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 
    deriving (Eq,Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat 
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

--11
data TalkToMe a = 
     Halt 
    |Print String a
    |Read (String -> a) 

instance Functor TalkToMe where 
    fmap _ Halt = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read ga) = Read (fmap f ga)

--Tests!
functorCompose' :: (Eq (f c),Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g.f) x) == (fmap g . fmap f $ x)

functorIdentity :: (Functor f,Eq(f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

type IntToInt = Fun Int Int
type QuantFI = Quant Int Int -> Bool
type QuantFC = Quant Int Int -> IntToInt -> IntToInt -> Bool
type KFI = K Int Int -> Bool
type KFC = K Int Int -> IntToInt -> IntToInt -> Bool
type EvilGoatFI = EvilGoateeConst Int Int -> Bool
type EvilGoatFC = EvilGoateeConst Int Int -> IntToInt -> IntToInt -> Bool


tests::IO()
tests = do 
       quickCheck (functorCompose' :: QuantFC)
       quickCheck (functorIdentity :: QuantFI) 
       quickCheck (functorCompose' :: KFC)
       quickCheck (functorIdentity :: KFI)
       quickCheck (functorCompose' :: EvilGoatFC)
       quickCheck (functorIdentity :: EvilGoatFI)


