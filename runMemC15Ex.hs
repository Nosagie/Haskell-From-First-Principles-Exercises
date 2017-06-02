module RunMemC15Ex where
--Monoid Exercise 10,conceptually challenging
import Data.Monoid

newtype Mem s a = 
    Mem {
       runMem :: s -> (a,s)
    }

instance Monoid a => Monoid (Mem s a) where
     mempty = Mem $ (\s -> (mempty,s)) 
     mappend (Mem {runMem = f}) (Mem {runMem = g}) = 
        Mem $ (\x -> let 
                    (u,q) = f x
                    (v,w) = g q
                    in
                    (u <> v,w))
                           

f' = Mem $ (\s -> ("hi",s+1))

testMem = do 
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String,Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0