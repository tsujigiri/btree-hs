import Test.Hspec

import BTree

main :: IO ()
main = hspec $ do
    describe "insert" $ do
        it "inserts a value into a tree" $ do
            let value = (1::Int)
                tree = insert value empty in
                member value tree `shouldBe` True

    describe "balance" $ do
        it "balances a tree" $ do
            let tree = foldl (flip insert) empty ([1,2,3]::[Int]) in
                (balance tree) `shouldBe` (foldl (flip insert) empty [2,1,3])

    describe "==" $ do
        it "returns True if two trees ar equal" $ do
            foldl (flip insert) empty ([1,2,3,4,5]::[Int])
            == foldl (flip insert) empty ([1,2,3,4,5]::[Int])
            `shouldBe` True

        it "returns False if two trees are different" $ do
            foldl (flip insert) empty ([1,2,3,4,5]::[Int])
            == foldl (flip insert) empty ([1,2,4,3,5]::[Int])
            `shouldBe` False

    describe "fromList" $ do
        it "returns a balanced tree" $ do
            fromList [1,2,3,4,5] `shouldBe` foldl (flip insert) empty ([1,2,3,4,5]::[Int])

    describe "values" $ do
        it "returns a list of the tree's values " $ do
            ((values . fromList) [1,2,3,4,5]::[Int]) `shouldBe` [1,2,3,4,5]


 
