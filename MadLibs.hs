import Data.Monoid
import Test.QuickCheck
import Control.Monad

type Verb = String 
type Adjective = String 
type Adverb = String
type Noun = String
type Exclamation = String 

madlibbin' :: Exclamation -> Adverb
              -> Noun -> Adjective
              -> String
madlibbin' e adv noun adj = 
   mconcat  [e,"! he said ",
   adv,"as he jumped into his car ",
   noun," and drove off with this ",
   adj," wife."]

monoidAssoc :: (Eq m,Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m,Monoid m)=> m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m,Monoid m)=> m -> Bool
monoidRightIdentity a = (mempty <> a) == a  

data Bull = Fools | Twoo 
    deriving (Eq,Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1,return Fools),(1,return Twoo)]

instance Monoid Bull where 
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity ::Bull -> Bool)

