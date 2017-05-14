module OperatingSystem where

data OperatingSystem = 
      GnuPluslinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq,Show)

data ProgrammingLanguage = 
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq,Show)

data Programmer = 
    Programmer {os :: OperatingSystem,
      lang :: ProgrammingLanguage}
      deriving (Eq,Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac,
                    lang = Haskell}


feelingWizardly :: Programmer
feelingWizardly = Programmer {lang =Agda
                        ,os = GnuPluslinux}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
    [ GnuPluslinux,
      OpenBSDPlusNevermindJustBSDStill,
      Mac,
      Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell,Agda,Idris,PureScript]

allProgrammers :: [Programmer]
allProgrammers = mapProgs allOperatingSystems 
        where
            mapProgs [] = []
            mapProgs (x:xs) = map (\y -> Programmer x y) allLanguages ++ mapProgs xs

-- use partially applied data constructors instead of partially applied records ie
data ThereYet = There Integer Float String Bool
            deriving (Eq,Show)


nope :: Float -> String -> Bool -> ThereYet 
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohooo"

yussss :: ThereYet
yussss = notQuite False

-- do not percorate values through bottoms
