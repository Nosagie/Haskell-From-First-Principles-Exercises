module C12Ex where

import Data.Char
import Data.List

-- to use binarytree in chapter 11
import Tree

--1 kind of a is a :: *
-- 2 kind of r :: a -> f a, a is :: *, f is * :: *


-- String processing
-- replaces all "the" with "a"
replaceThe::String -> String 
replaceThe = replaceTheHelper . words

notThe :: String -> Maybe String
notThe s = case s of 
            "the" -> Nothing
            _    -> Just s

replaceTheHelper :: [String] -> String
replaceTheHelper [] = ""
replaceTheHelper (x:xs) = 
    case (notThe x) of
            Nothing -> "a" ++ replaceTheHelper xs 
            Just s  -> " " ++ s ++ replaceTheHelper xs 


-- counts number of instances of "the" followed by vowel
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = cbvH . words

cbvH :: [String] -> Integer
cbvH [] = 0
cbvH wS@(x:xs)
            | (length wS) >= 2 =
                if (&&) (x == "the") . elem ((wS !! 1)!!0) 
                    $ ['a','e','i','o','u'] 
                    then 1 + cbvH xs 
                    else 0 + cbvH xs  
            | otherwise = 0

-- counts number of vowels in string
countVowels :: String -> Integer
countVowels = foldr ((+).isVowel) 0 

isVowel :: Char -> Integer
isVowel c = if elem (toLower c) ['a','e','i','o','u'] then 1 else 0


-- Validate the word
newtype Word' = Word' String deriving (Eq,Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = let 
            sLen = toInteger . length $ s 
            nVow = countVowels s 
            nCon = sLen - nVow
           in 
            if nVow > nCon then Nothing
            else Just . Word' $ s    


-- its only Natural
data Nat = Zero | Succ Nat
            deriving (Eq,Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat n
        | n < 0 = Nothing
        | n == 0 = Just Zero
        | otherwise = Just . Succ 
                    . fromJust . integerToNat
                    . (-) n $ 1

-- helper function for above, but available in Data.Maybe
fromJust :: Maybe a -> a 
fromJust Nothing = error "Value is Nothing"
fromJust (Just x) = x

-- Small library for maybe
isJust :: Maybe a -> Bool
isJust v = case v of 
        (Just _) -> True
        _        -> False

isNothing :: Maybe a -> Bool
isNothing v = case v of 
        Nothing -> True
        _       -> False


mayybe :: b -> (a -> b) -> Maybe a -> b 
mayybe s _ Nothing = s 
mayybe s f (Just x) = f x 

fromMaybe :: a -> Maybe a -> a 
fromMaybe v m = mayybe v id m  --using catamorphism

listToMaybe :: [a] -> Maybe a 
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes l = foldr ((++).isNothing') [] l 

isNothing' :: Maybe a -> [a]
isNothing' Nothing = []
isNothing' (Just x) = [x] 

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe l = let 
        hasNoth = foldr (&&) True . map isJust $ l 
        in 
        case hasNoth of 
            False -> Nothing 
            True -> Just . catMaybes $ l
        
-- Small Library for Either
lefts' :: [Either a b] -> [a]
lefts'  = foldr 
           ((++).(\x -> case x of 
                    (Left v) -> [v]
                    otherwise -> []))
           []  

rights' :: [Either a b] -> [b]
rights' = foldr
            ((++).(\x -> case x of
                    (Right v) -> [v]
                    otherwise -> []))
            []


partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' l = (lefts' l,rights' l)


eitherMaybe' :: (b->c) -> Either a b -> Maybe c 
eitherMaybe' f x = case x of 
            (Left _) -> Nothing
            (Right v) -> Just . f $ v

either' :: (a->c) -> (b->c) -> Either a b -> c 
either' f g x = case x of 
            (Left l) -> f l 
            (Right r) -> g r 

eitherMaybe'' :: (b->c) -> Either a b -> Maybe c 
eitherMaybe'' f x = case x of
            (Left _) -> Nothing
            (Right v) -> Just . f $ v   

-- writing your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f v = [v] ++ myIterate f (f v)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f v = case (f v) of 
        (Just (x,y)) -> [x] ++ myUnfoldr f y 
        otherwise -> [] 

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\v -> Just (v,f v)) x

-- FInally, something other than a list!
unfold :: (a -> Maybe(a,b,a)) -> a -> BinaryTree b 
unfold f s = case (f s) of 
        (Just (x,y,z)) -> Node (unfold f x) y (unfold f z)
        otherwise -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild s = unfold (\x -> if x <= s
                then Just(x+1,x,x+1) else Nothing)
                0




