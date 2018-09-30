import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  tree <- testSpec "spec" spec
  defaultMain tree
  -- hspec spec

spec :: Spec
spec = do
  it "success" ('a' == 'a')
  it "pending" pending
  it "pendingWith" (pendingWith "foo")
  it "failure (no reason)" ('a' == 'b')
  it "failure (reason)" (expectationFailure "foo")
  it "failure (expected but got)" ('a' `shouldBe` 'b')
  it "failure (uncaught exception)" (fail "nope" :: IO ())
  -- fit "focused" ('a' == 'a')
