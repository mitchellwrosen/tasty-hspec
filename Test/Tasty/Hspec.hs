{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Hspec
    ( testCase

    -- * Re-exports
    , module Test.Hspec
    , Test.Tasty.TestName
    , Test.Tasty.TestTree
    ) where

import           Control.Applicative ((<$>))
import           Data.Typeable       (Typeable)
import           Test.Hspec          (Spec)
import           Test.Hspec.Core
import           Test.Hspec.Runner   (Config(..), defaultConfig)
import           Test.QuickCheck     (Args)

import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.Providers (IsTest(..), singleTest, testPassed, testFailed)
import qualified Test.Tasty.Providers as T

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
testCase name = testGroup name . map specTreeToTestTree . runSpecM

-- Convert a hspec SpecTree into a tasty TestTree.
specTreeToTestTree :: SpecTree -> TestTree
specTreeToTestTree (SpecGroup name specs) = testGroup name (map specTreeToTestTree specs)
specTreeToTestTree (SpecItem (Item _ name example)) = singleTest name (MyExample example)

newtype MyExample = MyExample (Params -> (IO () -> IO ()) -> IO Result) deriving Typeable

instance IsTest MyExample where
    run _ (MyExample f) _ = hspecResultToTastyResult <$> f params id
      where
        hspecResultToTastyResult :: Result -> T.Result
        hspecResultToTastyResult Success     = testPassed ""
        hspecResultToTastyResult (Pending _) = testFailed "(test pending)"
        hspecResultToTastyResult (Fail str)  = testFailed str

        params :: Params
        params = Params quickCheckArgs smallCheckDepth reportProgress

        quickCheckArgs :: Args
        quickCheckArgs = configQuickCheckArgs defaultConfig

        smallCheckDepth :: Int
        smallCheckDepth = configSmallCheckDepth defaultConfig

        reportProgress :: Progress -> IO ()
        reportProgress _ = return ()

    testOptions = return []
