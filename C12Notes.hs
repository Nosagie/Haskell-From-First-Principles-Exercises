module C12Ex where

-- data Maybe a = Nothing | Just a

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just(n + 2) else Nothing


type Name = String 
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a


data Person = Person Name Age deriving Show 

data PersonInvalid = NameEmpty | 
                AgeTooLow deriving(Eq,Show)

-- smart data constructor
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age   
        | name /= "" && age >= 0 = Just $ Person name age
        | otherwise = Nothing


-- either 
-- data Either a b = Left a | Right b


mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age   
        | name /= "" && age >= 0 = Right $ Person name age
        | name == "" = Left NameEmpty
        | otherwise = Left AgeTooLow


ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of 
    True -> Right age 
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name 
nameOkay name = case name /= "" of 
    True -> Right name 
    False -> Left [NameEmpty]


mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = valid (nameOkay name) (ageOkay age)


valid :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
valid (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
valid (Left badName) (Left badAge) = Left (badName ++ badAge)
valid (Left badName) _ = Left badName
valid _ (Left badAge) = Left badAge 

-- lifted types can be bottom, but not unlifted 





















