module PatternMatchingC7 where

newtype Username = Username String

instance Eq Username where
    (==) (Username x) (Username y) = x == y
instance Ord Username where
    compare (Username x) (Username y) = if x > y then GT else
                                            if x == y then EQ else
                                                LT

newtype AccountNumber = AccountNumber Integer

instance Eq AccountNumber where
    (==) (AccountNumber x) (AccountNumber y) = x == y

data User = UnregisteredUser | RegisteredUser Username AccountNumber

instance Eq User where
     (==) (RegisteredUser _ y) (RegisteredUser _ y') = y == y'
     (==) UnregisteredUser _ = False
     (==) _ UnregisteredUser = False 

printUser :: User  -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
                            putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = Galapagos | Antarctic | Australia | SouthAfrica
                    | SouthAmerica 
        deriving (Eq,Show)

data Penguin = Peng WherePenguinsLive
            deriving (Eq,Show)

-- if South Africa return True
isItSouthAfrica :: WherePenguinsLive -> Bool
isItSouthAfrica SouthAfrica = True
isItSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives


galapagosPenguin :: Penguin -> Bool 
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctic) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool 
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)


-- Tuples
addEmUp2 :: Num a => (a,a) -> a 
addEmUp2 (x,y) = x + y

addEmUp2alt :: Num a => (a,a) -> a 
addEmUp2alt tup = (fst tup) + (snd tup)

fst3 :: (a,b,c) -> a 
fst3 (x,_,_) = x

third3 :: (a,b,c) -> c 
third3 (_,_,x) = x 

f' :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f' (a,_,c) (d,_,f) = ((a,d),(c,f))

funcZ :: Int -> [Char]
funcZ x = 
    case x + 1 == 1 of 
        True -> "Awesome"
        False -> "wut"

pal :: [Char] -> [Char]
pal xs = 
    case xs == reverse xs of
        True -> "Yes"
        False -> "No"

pal' :: [Char] -> [Char]
pal' xs = let rev = reverse xs in 
            if xs == rev then "Yes" else "No" 

pal'' :: [Char] -> Bool
pal'' xs = (==) xs rev where rev = reverse xs  

greetIfCool :: String -> IO()
greetIfCool coolness = 
    case cool of 
        True -> putStrLn "eyyy. What's shakin'?"
        False -> putStrLn "pshhhhh."
    where cool = coolness == "downright frosty yo"

data Employee = Coder 
            | Manager
            | Veep
            | CEO 
         deriving (Eq,Ord,Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) ->Employee -> Employee -> IO()
employeeRank f e e' =
    case f e e' of 
        GT -> reportBoss e e' 
        EQ -> putStrLn "Neither Employee is the boss"
        LT -> reportBoss e' e

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ 
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT 
codersRuleCEOsDrool e e' = compare e e'

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne ::(Num a) => a -> a 
oneIsOne = dodgy 1

oneIsTwo ::(Num a) => (a -> a) 
oneIsTwo = (flip dodgy) 2





    


