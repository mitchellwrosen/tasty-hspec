{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Hspec
    ( testCase

    -- * Re-exports
    , module Test.Hspec
    , Test.Tasty.TestName
    , Test.Tasty.TestTree
    ) where

import Test.Hspec

import Data.Typeable        (Typeable)
import Test.Tasty           (TestName, TestTree)
import Test.Tasty.Providers (IsTest(..), Result(..), singleTest)
import Test.Hspec.Runner    (Summary(..), hspecResult)

-- | Turn an hspec @Spec@ into a tasty @TestTree@.
--
-- > module AnimalsSpec (tests) where
-- >
-- > import Test.Tasty.Hspec
-- >
-- > tests :: TestTree
-- > tests = testGroup "animals"
-- >     [ testCase "mammals" mammalsSpec
-- >     , testCase "birds"   birdsSpec
-- >     ]
-- >
-- > mammalsSpec :: Spec
-- > mammalsSpec = do
-- >     describe "cow" $ do
-- >         it "moos" $
-- >             speak cow `shouldBe` "moo"
-- >
-- >         it "eats grass" $
-- >             hungryFor cow `shouldBe` "grass"
-- >
-- > birdsSpec :: Spec
-- > birdsSpec = do
-- >     describe "ostrich" $ do
-- >         it "sticks its head in sand" $
-- >             fmap (`shouldBe` InSand) getHeadState
testCase :: TestName -> Spec -> TestTree
testCase name = singleTest name . MySpec

newtype MySpec = MySpec Spec deriving Typeable

instance IsTest MySpec where
    run _ (MySpec spec) _ = do
        (Summary examples failures) <- hspecResult spec
        return $ Result (failures == 0) ""
    testOptions = return []
