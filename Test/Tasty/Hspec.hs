{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Hspec (
    -- * Test
      testSpec
    -- * Options
    -- | === Re-exported from <https://hackage.haskell.org/package/tasty-smallcheck tasty-smallcheck>
    , SmallCheckDepth(..)
    -- | === Re-exported from <https://hackage.haskell.org/package/tasty-quickcheck tasty-quickcheck>
    , QuickCheckMaxRatio(..)
    , QuickCheckMaxSize(..)
    , QuickCheckReplay(..)
    , QuickCheckTests(..)
    -- * Hspec re-export
    , module Test.Hspec
    ) where

import           Control.Applicative    ((<$>))
import           Control.Monad          (join)
import           Data.Proxy
import           Data.Typeable          (Typeable)
import qualified Test.Hspec             as H
import qualified Test.Hspec.Core.Spec   as H
import qualified Test.Hspec.Runner      as H
import qualified Test.QuickCheck        as QC
import qualified Test.QuickCheck.Random as QC
import qualified Test.Tasty             as T
import qualified Test.Tasty.SmallCheck  as TSC
import qualified Test.Tasty.Options     as T
import qualified Test.Tasty.Providers   as T
import qualified Test.Tasty.QuickCheck  as TQC
import           System.Random

-- For re-export.
import Test.Hspec
import Test.Tasty.SmallCheck (SmallCheckDepth(..))
import Test.Tasty.QuickCheck (QuickCheckMaxRatio(..), QuickCheckMaxSize(..)
                             , QuickCheckReplay(..), QuickCheckTests(..))

-- | Create a <https://hackage.haskell.org/package/tasty tasty> 'T.TestTree' from an
-- <https://hackage.haskell.org/package/hspec Hspec> 'H.Spec'.
testSpec :: T.TestName -> H.Spec -> IO T.TestTree
testSpec name spec = H.runSpecM spec >>= fmap (T.testGroup name . join) . mapM specTreeToTestTrees

specTreeToTestTrees :: H.SpecTree () -> IO [T.TestTree]
specTreeToTestTrees (H.Node name spec_trees) = do
    test_trees <- join <$> mapM specTreeToTestTrees spec_trees
    return [T.testGroup name test_trees]
specTreeToTestTrees (H.NodeWithCleanup cleanup spec_trees) = do
    cleanup ()
    join <$> mapM specTreeToTestTrees spec_trees
specTreeToTestTrees (H.Leaf item) = return [T.singleTest (H.itemRequirement item) (Item item)]

hspecResultToTastyResult :: H.Result -> T.Result
hspecResultToTastyResult H.Success        = T.testPassed ""
hspecResultToTastyResult (H.Pending mstr) = T.testFailed ("test pending" ++ maybe "" (": " ++) mstr)
hspecResultToTastyResult (H.Fail str)     = T.testFailed str

newtype Item = Item (H.Item ())
    deriving Typeable

instance T.IsTest Item where
    run opts (Item (H.Item _ _ _ example)) progress =
        hspecResultToTastyResult <$> example params ($ ()) hprogress
      where
        params :: H.Params
        params = H.Params
            { H.paramsQuickCheckArgs = qc_args
            , H.paramsSmallCheckDepth = sc_depth
            }
          where
            qc_args :: QC.Args
            qc_args =
                let TQC.QuickCheckTests    num_tests = T.lookupOption opts
                    TQC.QuickCheckReplay   replay    = T.lookupOption opts
                    TQC.QuickCheckMaxSize  max_size  = T.lookupOption opts
                    TQC.QuickCheckMaxRatio max_ratio = T.lookupOption opts
                in QC.stdArgs
                    { QC.chatty          = False
                    , QC.maxDiscardRatio = max_ratio
                    , QC.maxSize         = max_size
                    , QC.maxSuccess      = num_tests
                    , QC.replay          = replay
                    }

            sc_depth :: Int
            sc_depth = let TSC.SmallCheckDepth depth = T.lookupOption opts in depth

        hprogress :: H.Progress -> IO ()
        hprogress (x,y) = progress $ T.Progress
            { T.progressText    = ""
            , T.progressPercent = fromIntegral x / fromIntegral y
            }

    testOptions = return
        [ T.Option (Proxy :: Proxy TQC.QuickCheckTests)
        , T.Option (Proxy :: Proxy TQC.QuickCheckReplay)
        , T.Option (Proxy :: Proxy TQC.QuickCheckMaxSize)
        , T.Option (Proxy :: Proxy TQC.QuickCheckMaxRatio)
        , T.Option (Proxy :: Proxy TSC.SmallCheckDepth)
        ]
