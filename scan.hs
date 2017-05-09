module Scan where


fibs = 1 : scanl (+) 1 fibs
fibsN x = (!!) fibs x

myfibs = take 20 fibs 

myfibs' = takeWhile (<100) fibs

fact = scanl (*) 1 [1..]
factN = (!!) fact

