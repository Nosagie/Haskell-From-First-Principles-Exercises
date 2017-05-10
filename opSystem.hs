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