import Data.Time

data DatabaseItem = DbString String 
                    | DbNumber Integer
                    | DbDate UTCTime
                deriving (Eq,Ord,Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [
        DbDate ( UTCTime 
                ( fromGregorian 1911 5 1)
                ( secondsToDiffTime 34123)
                )
        ,DbString "Hello World!"
        ,DbDate ( UTCTime 
                    (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123)
                )
        ,DbNumber 45,DbNumber 68
    ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x:xs) = f x ++ filterDbDate xs
    where 
        f :: DatabaseItem -> [UTCTime]
        f (DbDate t) = [t]
        f _          = []    


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (x:xs) = f x ++ filterDbNumber xs
    where 
        f :: DatabaseItem -> [Integer]
        f (DbNumber t) = [t]
        f _            = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent l  = foldr 
                    (\x y -> if x > y then x else y) 
                    (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) 
                    (filterDbDate l)

sumDb :: [DatabaseItem] -> Integer
sumDb l  = sum . filterDbNumber $ l

avgDb :: [DatabaseItem] -> Double
avgDb l = let 
            total = sumDb l;
            listSize = length . filterDbNumber $ l;
          in   
            (fromIntegral total)/(fromIntegral listSize)
