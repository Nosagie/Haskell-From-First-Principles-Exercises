module C15Ex where

import Data.Semigroup
import Test.QuickCheck hiding (Success,Failure)

--1
data Trivial = Trivial deriving (Eq,Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

--2
newtype Identity a = Identity a
       deriving (Eq,Show)

instance Semigroup a => Semigroup (Identity a) where
   (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do
         x <- arbitrary
         return (Identity x)

--3
data Two a b = Two a b deriving (Eq,Show)

instance (Semigroup a,Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two v z) = Two (x <> v) (y <> z)

instance  (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
     arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        return (Two x y) 

--4
data Three a b c = Three a b c deriving (Eq,Show)

instance (Semigroup a,Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three u v w) = Three (x <> u)                             (y <> v) (z <> w)

instance (Arbitrary a,Arbitrary b,Arbitrary c) => Arbitrary (Three a b c) where
     arbitrary = do
          x <- arbitrary
          y <- arbitrary
          z <- arbitrary
          return (Three x y z)
--5
data Four a b c d = Four a b c d deriving (Eq,Show)

instance (Semigroup a,Semigroup b,Semigroup c,Semigroup d) => Semigroup (Four a b c d) where 
    (Four w x y z) <> (Four t s u i) = Four (w <> t) (x <> s) (y <> u) (z <> i)

instance (Arbitrary a,Arbitrary b,Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
     arbitrary = do 
          w <- arbitrary
          x <- arbitrary
          y <- arbitrary
          z <- arbitrary
          return (Four w x y z)
 
--6
newtype BoolConj = BoolConj Bool deriving (Eq,Show)
         
instance Semigroup BoolConj where 
    (BoolConj False) <> (BoolConj _) = BoolConj False
    (BoolConj _)  <> (BoolConj False) = BoolConj False
    (BoolConj _)  <> (BoolConj _) = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = do 
        x <- arbitrary
        return (BoolConj x)

--7
newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)

instance Semigroup BoolDisj where 
    (BoolDisj True) <> (BoolDisj _) = BoolDisj True
    (BoolDisj _) <> (BoolDisj True) = BoolDisj True
    (BoolDisj _) <> (BoolDisj _) = BoolDisj False

instance Arbitrary BoolDisj where 
    arbitrary = do 
       x <- arbitrary
       return (BoolDisj x)

--8 
data Or a b = Fst a | Snd b deriving (Eq,Show)

instance Semigroup (Or a b) where 
   x@(Snd _) <> _ = x
   _ <> x@(Fst _) = x
   _ <> x@(Snd _) = x

instance (Arbitrary a,Arbitrary b) => Arbitrary (Or a b) where
   arbitrary = do
      x <- arbitrary 
      y <- arbitrary
      elements $ [Fst x,Snd y]   

--9
newtype Combine a b = Combine {unCombine :: (a->b)}          

instance (Semigroup b) => Semigroup(Combine a b)where
   (Combine {unCombine = f}) <> (Combine {unCombine = g}) = Combine (f <> g)


--10
newtype Comp a = Comp {unComp  :: (a -> a)}

instance (Semigroup a) => Semigroup (Comp a) where
 (Comp {unComp = f}) <> (Comp {unComp = g}) = Comp (f <> g)

--11
data Validation a b = Failure a | Success b
     deriving (Eq,Show)

instance Semigroup a => Semigroup (Validation a b) where 
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Success x) <> _  = Success x
  _ <> (Success y) = Success y 

instance (Arbitrary a,Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      elements $ [Failure x, Success y]

--12 
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq,Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where 
   (AccumulateRight (Success x)) <> (AccumulateRight (Success x')) = (AccumulateRight (Success (x <> x')))



--13
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq,Show) 

instance (Semigroup a,Semigroup b) => Semigroup (AccumulateBoth a b) where 
   (AccumulateBoth (Success x)) <> (AccumulateBoth (Success x')) = (AccumulateBoth ((Success<>Success)(x <> x')))
   (AccumulateBoth (Success x)) <> (AccumulateBoth (Failure x')) = (AccumulateBoth ((Success x <>Failure x')))
   (AccumulateBoth (Failure x)) <> (AccumulateBoth (Success x')) = (AccumulateBoth ((Failure x <>Success x')))
   (AccumulateBoth (Failure x)) <> (AccumulateBoth (Failure x')) = (AccumulateBoth ((Failure x <>Failure x')))



type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool 
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool
type BoolConjAssoc = BoolConj  -> BoolConj  -> BoolConj  -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool 
type CombAssoc = Combine String Ordering -> Combine String Ordering -> Combine String Ordering -> Bool
type ValAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

semiGroupAssoc :: (Eq m,Semigroup m) => m -> m -> m-> Bool
semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO()
main = do  
        quickCheck (semiGroupAssoc :: TrivialAssoc)
        quickCheck (semiGroupAssoc :: IdentityAssoc [Int])
        quickCheck (semiGroupAssoc :: TwoAssoc [Int] String)
        quickCheck (semiGroupAssoc :: ThreeAssoc [Int] (Identity [Int]) (Two String [String]))
        quickCheck (semiGroupAssoc :: FourAssoc [Int] String String [String])
        quickCheck (semiGroupAssoc :: BoolConjAssoc)
        quickCheck (semiGroupAssoc :: BoolDisjAssoc)
        quickCheck (semiGroupAssoc :: OrAssoc String Char)
        quickCheck (semiGroupAssoc :: ValAssoc)


