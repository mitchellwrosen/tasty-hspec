{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}

module Test.Tasty.Hspec (
    -- * Test
      testSpec
    , testSpecs
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

import Control.Applicative ((<$>))
import Data.Proxy
import Data.Tagged (Tagged)
import Data.Typeable (Typeable)

import qualified Test.Hspec as H
import qualified Test.Hspec.Core.Formatters as H
import qualified Test.Hspec.Core.Spec as H
import qualified Test.QuickCheck as QC
#if MIN_VERSION_tasty_quickcheck(0,9,0)
import qualified Test.QuickCheck.Random as QCR
#endif
import qualified Test.Tasty as T
import qualified Test.Tasty.SmallCheck as TSC
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Providers as T
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.Tasty.Runners as T

-- For re-export.
import Test.Hspec
import Test.Tasty.SmallCheck (SmallCheckDepth(..))
import Test.Tasty.QuickCheck
  (QuickCheckMaxRatio(..), QuickCheckMaxSize(..), QuickCheckReplay(..),
    QuickCheckTests(..))

-- | Create a <https://hackage.haskell.org/package/tasty tasty> 'T.TestTree' from an
-- <https://hackage.haskell.org/package/hspec Hspec> 'H.Spec'.
testSpec :: T.TestName -> H.Spec -> IO T.TestTree
testSpec name spec = T.testGroup name <$> testSpecs spec

-- | Create a list of <https://hackage.haskell.org/package/tasty tasty>
-- 'T.TestTree' from a <https://hackage.haskell.org/package/hspec Hspec>
-- 'H.Spec' test. This returns the same tests as 'testSpec' but doesn't create
-- a <https://hackage.haskell.org/package/tasty tasty> test group from them.
testSpecs :: H.Spec -> IO [T.TestTree]
testSpecs spec = map specTreeToTestTree <$> H.runSpecM spec

specTreeToTestTree :: H.SpecTree () -> T.TestTree
specTreeToTestTree (H.Node name spec_trees) =
  T.testGroup name (map specTreeToTestTree spec_trees)
specTreeToTestTree (H.NodeWithCleanup cleanup spec_trees) =
  T.WithResource (T.ResourceSpec (return ()) cleanup) (const test_tree)
 where
  test_tree :: T.TestTree
  test_tree = specTreeToTestTree (H.Node "(unnamed)" spec_trees)
specTreeToTestTree (H.Leaf item) = T.singleTest (H.itemRequirement item) (Item item)

newtype Item = Item (H.Item ())
    deriving Typeable

instance T.IsTest Item where
  run :: T.OptionSet -> Item -> (T.Progress -> IO ()) -> IO T.Result
  run opts (Item (H.Item _ _ _ ex)) progress =
#if MIN_VERSION_hspec(2,4,0)
    either (T.testFailed . H.formatException) hspecResultToTastyResult
#else
    hspecResultToTastyResult
#endif
      <$> ex params ($ ()) hprogress
   where
    params :: H.Params
    params = H.Params
      { H.paramsQuickCheckArgs = qc_args
      , H.paramsSmallCheckDepth = sc_depth
      }
     where
      qc_args :: QC.Args
      qc_args = QC.stdArgs
        { QC.chatty          = False
        , QC.maxDiscardRatio = max_ratio
        , QC.maxSize         = max_size
        , QC.maxSuccess      = num_tests
#if MIN_VERSION_tasty_quickcheck(0,9,0)
        , QC.replay          = (\seed -> (QCR.mkQCGen seed, seed)) <$> replay
#else
        , QC.replay          = replay
#endif
        }
       where
        TQC.QuickCheckTests    num_tests = T.lookupOption opts
        TQC.QuickCheckReplay   replay    = T.lookupOption opts
        TQC.QuickCheckMaxSize  max_size  = T.lookupOption opts
        TQC.QuickCheckMaxRatio max_ratio = T.lookupOption opts

      sc_depth :: Int
      sc_depth = depth
       where
        TSC.SmallCheckDepth depth = T.lookupOption opts

    hprogress :: H.Progress -> IO ()
    hprogress (x,y) = progress (T.Progress
      { T.progressText    = ""
      , T.progressPercent = fromIntegral x / fromIntegral y
      })

  testOptions :: Tagged Item [T.OptionDescription]
  testOptions = return
    [ T.Option (Proxy :: Proxy TQC.QuickCheckTests)
    , T.Option (Proxy :: Proxy TQC.QuickCheckReplay)
    , T.Option (Proxy :: Proxy TQC.QuickCheckMaxSize)
    , T.Option (Proxy :: Proxy TQC.QuickCheckMaxRatio)
    , T.Option (Proxy :: Proxy TSC.SmallCheckDepth)
    ]

hspecResultToTastyResult :: H.Result -> T.Result
hspecResultToTastyResult H.Success = T.testPassed ""
hspecResultToTastyResult (H.Pending mstr) = T.testFailed ("Test pending" ++ maybe "" (": " ++) mstr)
#if MIN_VERSION_hspec(2,4,0)
hspecResultToTastyResult (H.Failure _ reason) =
  case reason of
    H.NoReason   -> T.testFailed ""
    H.Reason str -> T.testFailed str
    H.ExpectedButGot preface expected actual ->
      T.testFailed $ mconcat
        [ maybe "" (++ ": ") preface
        , "expected " ++ expected
        , ", but got " ++ actual
        ]
#elif MIN_VERSION_hspec(2,2,0)
hspecResultToTastyResult (H.Fail _ str) = T.testFailed str
#else
hspecResultToTastyResult (H.Fail str) = T.testFailed str
#endif
