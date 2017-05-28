import Control.Monad
import Data.Char(toLower,isLetter)
import System.Exit (exitSuccess)


palindrome :: IO()
palindrome = forever $ do
    line <- getLine
    case ((filter isLetter $ map toLower line) == reverse (filter isLetter $ map toLower line)) of 
        True -> putStrLn "It's a palindrome!"
        False -> do 
                   putStrLn "Nope!"
                   exitSuccess


-- num 4
type Name = String 
type Age = Integer 

data Person = Person Name Age deriving Show 

data PersonInvalid = NameEmpty
                    | AgeTooLow 
                    | PersonInvalidUnknown String 
                    deriving (Eq,Show)


mkPerson :: Name 
          -> Age 
          -> Either PersonInvalid Person
mkPerson name age 
    | name /= "" && age > 0 = Right $ Person name age 
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                    "Name was: " ++ show name ++
                    " Age was: " ++ show age 


gimmePerson :: IO()
gimmePerson = do 
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    age <- getLine
    case (mkPerson name (read age::Integer)) of 
        (Right x) -> 
                putStrLn $ "Yay! Successfully got a person: " ++ (show x)
        (Left error) -> 
                putStrLn $ "Error occured: " ++ (show error)






