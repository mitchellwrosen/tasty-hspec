import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  tree <- testSpec "hspec tests" hspecSuite
  defaultMain tree

hspecSuite :: Spec
hspecSuite = do
  describe "passing test" $
    it "5 == 5" $
      5 `shouldBe` (5 :: Int)

  describe "pending test" $
    it "pending" $
      pending

  describe "failing test" $
    it "5 == 6" $
      5 `shouldBe` (6 :: Int)
