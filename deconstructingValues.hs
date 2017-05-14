module DeconstructingValues where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show


-- FarmerType is a Sum
data FarmerType = DairyFarmer 
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True 
isDairyFarmer _ = False 

-- record syntax
data FarmerRec =
    FarmerRec { name :: Name
            , acres :: Acres 
            , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool 
isDairyFarmerRec farmer = case farmerType farmer of 
    DairyFarmer -> True
    _           -> False

-- don't use Null in data constructor
-- the typechecker best helps those who help themselves
-- use Maybes instead of Null

data Silly a b c d = MkSilly a b c d deriving Show 

-- :kind Silly is * -> * -> * -> *

