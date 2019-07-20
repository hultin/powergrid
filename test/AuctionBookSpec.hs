module AuctionBookSpec where

import AuctionBook
import Data.List
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "AuctionBook" $ do
    describe "dropWorst" $ do
      it "[3,1,4] gives [3,4]" $ do dropWorst True [3, 1, 4] `shouldBe` [3, 4]
      it "no drop if False" $ property $ \xs -> dropWorst False xs == xs
      it "if true, length -1" $
        forAll (listOf1 arbitrary) $ \xs ->
          length (dropWorst True xs) == length xs - 1
      it "if true, lowest is gone" $
        forAll (listOf1 arbitrary) $ \xs ->
          dropWorst True xs == drop 1 (sort xs)
      it "empty list" $ property $ \b -> dropWorst b [] == []
      
    describe "bestToDeck" $ do
      it "[3,1,4] [100,101] gives ([100,101,4], [3, 1])" $ do bestToDeck [3,1,4] [100,101] `shouldBe` ([100,101,4], [3, 1])
      it "empty book" $ property $ \xs -> bestToDeck [] xs == (xs, [])
      it "" $ forAll (listOf1 arbitrary :: Gen [Int]) $ \bs ->
              property $ \ds ->
              let (newds, newbs) = bestToDeck bs ds
                in length bs + length ds == length newbs + length newds
 
    describe "fillAuctionBook" $ do
      it "allways == 8" $
        forAll (vectorR (8, 30) :: Gen [Int]) $ \deck ->
        forAll (vectorR (0, 8)  :: Gen [Int]) $ \book ->
          let (newds, newbs) = fillAuctionBook deck book
            in length newbs == 8
      it "elements are moving" $ 
        forAll (vectorR (8, 30) :: Gen [Int]) $ \deck ->
        forAll (vectorR (0, 8)  :: Gen [Int]) $ \book ->
          let (newds, newbs) = fillAuctionBook deck book
            in book ++ deck == newbs ++ newds
      it "elements are moving" $ verbose $
        forAll (listOf arbitrary :: Gen [Int]) $ \deck ->
        forAll (listOf arbitrary :: Gen [Int]) $ \book ->
          let (newds, newbs) = fillAuctionBook deck book
            in length book + length deck == length newbs + length newds
    
            -- it "" $ forAll (listOf1 arbitrary :: Gen [Int]) $ \bs ->
        -- length bs == 0

-- | Generates a list of random length. The maximum length depends on the
-- size parameter.
vectorR :: Arbitrary a => (Int, Int) -> Gen [a]
vectorR r = listOfR r arbitrary

listOfR :: (Int, Int) -> Gen a -> Gen [a]
listOfR (a, b) gen = do
     k <- choose (a,b)
     vectorOf k gen