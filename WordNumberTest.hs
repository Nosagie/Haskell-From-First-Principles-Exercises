module WordNumberTest where 

import Test.Hspec
import WordNumber (digitToWord,digits,wordNumber)

main :: IO()
main = hspec $ do
    describe "digittoword does what we want" $ do
      it "returns zero for 0" $ do
         digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do
         print "one is 1"

    describe "digits does what we want" $ do
      it "returns [1] for 1" $ do 
         digits 1 `shouldBe` [1] 
         print "1 is [1]"

    describe "wordNumber does what we want" $ do
      it "returns one-zero-zerofor 100" $ do
         wordNumber 100 `shouldBe` "one-zero-zero" 
      it "returns nine-zero-zero-one for 9001" $ do
         print "9001 is nine-zero-zero-one"

