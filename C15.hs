module C15 where 

import Data.Monoid
import Test.QuickCheck


--A Monoid is a binary associative operation with an identity

--typeclass Monoid definition
--class Monoid' m where 
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

data Optional a = 
   Nada
  |Only a
  deriving (Eq,Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend x@(Only _) Nada = x
  mappend Nada Nada = Nada
  mappend Nada x@(Only _) = x
  mappend (Only x) (Only y) = Only (mappend x y)
 
--Exercise for intermission
newtype First' a = First' { getFirst' :: Optional a}
   deriving (Eq,Show)

instance Monoid (First' a) where
   mempty = First' Nada
   mappend x@(First' Nada) (First' Nada) = x
   mappend x@(First' (Only _)) (First' Nada) = x
   mappend (First' Nada) x@(First' (Only _)) = x
   mappend (First' x@(Only _)) (First' y@(Only _)) = First' x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return (First' (Only x)))
              , (1, return (First' Nada))]

type FirstMappend = First' String -> First' String -> First' String -> Bool

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

monoidAssoc :: (Eq m,Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m,Monoid m)=> m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m,Monoid m)=> m -> Bool
monoidRightIdentity a = (mempty <> a) == a

main :: IO()
main = do 
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
 
