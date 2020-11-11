{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | @hspec@ and @tasty@ serve similar purposes; consider using one or the
-- other.
--
-- However, in a pinch, this module allows you to run an @hspec@ 'H.Spec' as a
-- @tasty@ 'T.TestTree'.

module Test.Tasty.Hspec
    ( -- * Tests
      testSpec
    , testSpecs
    -- * Options
    , TreatPendingAs(..)
    -- * Re-exports
    , module Test.Hspec
      -- * Examples
      -- $examples
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Monad (guard)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Proxy
import Data.Typeable (Typeable)

import qualified Test.Hspec as H
import qualified Test.Hspec.Core.Formatters as H
import qualified Test.Hspec.Core.Spec as H
import qualified Test.QuickCheck as QC
import qualified Test.Tasty as T
import qualified Test.Tasty.SmallCheck as TSC
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Providers as T
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.Tasty.Runners as T

-- For re-export.
import Test.Hspec

-- $examples
--
-- The simplest usage of this library involves first creating a 'T.TestTree' in
-- @IO@, then running it with 'T.defaultMain'.
--
-- @
-- main = do
--   spec <- 'testSpec' "spec" mySpec
--   'T.defaultMain'
--     ('T.testGroup' "tests"
--       [ spec
--       , ...
--       ])
-- @
--
-- You can treat an 'H.pending'/'H.pendingWith' test as a success instead of a
-- failure (the default):
--
-- @
-- tests :: TestTree
-- tests =
--   localOption TreatPendingAsSuccess $ testGroup "My Hspec TestTree"
--     $ [ unsafePerformIO (testSpec "My first Hspec test" spec_firstHspecTest)
--       , ...
--       ]
-- @
--
-- However, if you don't do any @IO@ during 'Spec' creation, or the @IO@ need
-- not be performed at any particular time relative to other @IO@ actions, it's
-- perfectly fine to use 'System.IO.unsafePerformIO'.
--
-- @
-- main = do
--   'T.defaultMain'
--     ('T.testGroup' "tests"
--       [ 'System.IO.unsafePerformIO' ('testSpec' "spec" mySpec)
--       , ...
--       ])
-- @

-- | Create a <https://hackage.haskell.org/package/tasty tasty> 'T.TestTree' from an
-- <https://hackage.haskell.org/package/hspec Hspec> 'H.Spec'.
testSpec :: T.TestName -> H.Spec -> IO T.TestTree
testSpec name spec = T.testGroup name <$> testSpecs spec

-- | Create a list of <https://hackage.haskell.org/package/tasty tasty>
-- 'T.TestTree' from an <https://hackage.haskell.org/package/hspec Hspec>
-- 'H.Spec'. This returns the same tests as 'testSpec' but doesn't create
-- a <https://hackage.haskell.org/package/tasty tasty> test group from them.
testSpecs :: H.Spec -> IO [T.TestTree]
testSpecs spec =
  catMaybes . map specTreeToTestTree <$>
    -- In hspec 2.6.0, "focus" was introduced. Here we do as hspec does, which
    -- is pre-process a spec by focusing the whole thing, which is a no-op if
    -- anything inside is already focused, but otherwise focuses every item.
    -- Then, when creating a tasty test tree, we just toss the unfocused items.
    H.runSpecM (doFocus spec)
  where
    doFocus :: H.Spec -> H.Spec
    doFocus =
#if MIN_VERSION_hspec(2,6,0)
      H.focus
#else
      id
#endif

specTreeToTestTree :: H.SpecTree () -> Maybe T.TestTree
specTreeToTestTree spec_tree =
  case spec_tree of
    H.Node name spec_trees ->
      Just (T.testGroup name (mapMaybe specTreeToTestTree spec_trees))
    H.NodeWithCleanup cleanup spec_trees ->
      T.WithResource (T.ResourceSpec (return ()) cleanup) . const <$> test_tree
     where
      test_tree :: Maybe T.TestTree
      test_tree = specTreeToTestTree (H.Node "(unnamed)" spec_trees)
    H.Leaf item -> do
      guard (hspecItemIsFocused item)
      Just (T.singleTest (H.itemRequirement item) (Item item))

newtype Item
  = Item (H.Item ())
  deriving Typeable

instance T.IsTest Item where
  -- run :: T.OptionSet -> Item -> (T.Progress -> IO ()) -> IO T.Result
  run opts (Item item) progress = do
    qc_args <- tastyOptionSetToQuickCheckArgs opts

    let
      pending_ :: String -> T.Result
      pending_ =
        case T.lookupOption opts of
          TreatPendingAsFailure -> T.testFailed
          TreatPendingAsSuccess -> T.testPassed

    let
      params :: H.Params
      params = H.Params
        { H.paramsQuickCheckArgs = qc_args
        , H.paramsSmallCheckDepth = sc_depth
        }

#if MIN_VERSION_hspec(2,4,0) && !MIN_VERSION_hspec(2,5,0)
    either handleUncaughtException (hspecResultToTastyResult pending_)
#else
    hspecResultToTastyResult pending_
#endif
      <$> hspecItemToExample item params ($ ()) hprogress

   where
    sc_depth :: Int
    sc_depth =
      case T.lookupOption opts of
        TSC.SmallCheckDepth depth ->
          depth

    hprogress :: H.Progress -> IO ()
    hprogress (x,y) =
      progress T.Progress
        { T.progressText    = ""
        , T.progressPercent = fromIntegral x / fromIntegral y
        }

  -- testOptions :: Tagged Item [T.OptionDescription]
  testOptions =
    return
      [ T.Option (Proxy :: Proxy TreatPendingAs)
      , T.Option (Proxy :: Proxy TQC.QuickCheckTests)
      , T.Option (Proxy :: Proxy TQC.QuickCheckReplay)
      , T.Option (Proxy :: Proxy TQC.QuickCheckMaxSize)
      , T.Option (Proxy :: Proxy TQC.QuickCheckMaxRatio)
      , T.Option (Proxy :: Proxy TSC.SmallCheckDepth)
      ]

tastyOptionSetToQuickCheckArgs :: T.OptionSet -> IO QC.Args
tastyOptionSetToQuickCheckArgs opts =
#if MIN_VERSION_tasty_quickcheck(0,9,1)
  snd <$> TQC.optionSetToArgs opts
#else
  return QC.stdArgs
    { QC.chatty          = False
    , QC.maxDiscardRatio = max_ratio
    , QC.maxSize         = max_size
    , QC.maxSuccess      = num_tests
    , QC.replay          = replay
    }
  where
    TQC.QuickCheckTests    num_tests = T.lookupOption opts
    TQC.QuickCheckReplay   replay    = T.lookupOption opts
    TQC.QuickCheckMaxSize  max_size  = T.lookupOption opts
    TQC.QuickCheckMaxRatio max_ratio = T.lookupOption opts
#endif

hspecResultToTastyResult :: (String -> T.Result) -> H.Result -> T.Result
#if MIN_VERSION_hspec(2,5,0)
hspecResultToTastyResult pending_ (H.Result _ result) =
#else
hspecResultToTastyResult pending_ result =
#endif
  case result of
    H.Success ->
      T.testPassed ""

#if MIN_VERSION_hspec(2,5,0)
    H.Pending _ x ->
#else
    H.Pending x ->
#endif
      handleResultPending pending_ x

#if MIN_VERSION_hspec(2,4,0)
    H.Failure _ x ->
      handleResultFailure x
#elif MIN_VERSION_hspec(2,2,0)
    H.Fail _ str -> T.testFailed str
#else
    H.Fail str -> T.testFailed str
#endif

hspecItemIsFocused :: H.Item a -> Bool
hspecItemIsFocused =
#if MIN_VERSION_hspec(2,6,0)
  H.itemIsFocused
#else
  const True
#endif

hspecItemToExample
  :: H.Item a
  -> H.Params
  -> (H.ActionWith a -> IO ())
  -> H.ProgressCallback
  -> IO H.Result
#if MIN_VERSION_hspec(2,6,0)
hspecItemToExample (H.Item _ _ _ _ ex) = ex
#else
hspecItemToExample (H.Item _ _ _ ex) = ex
#endif


handleResultPending :: (String -> T.Result) -> Maybe String -> T.Result
handleResultPending pending_ x =
  pending_ ("# PENDING: " ++ fromMaybe "No reason given" x)

-- FailureReason
--
-- - Introduced in 2.4.0
-- - Error constructor added in 2.5.0
#if MIN_VERSION_hspec(2,4,0)
handleResultFailure :: H.FailureReason -> T.Result
handleResultFailure reason =
  case reason of
    H.NoReason -> T.testFailed ""
    H.Reason x -> T.testFailed x
    H.ExpectedButGot preface expected actual ->
      T.testFailed . unlines . catMaybes $
        [ preface
        , Just ("expected: " ++ expected)
        , Just (" but got: " ++ actual)
        ]
#if MIN_VERSION_hspec(2,5,0)
    H.Error _ ex ->
      handleUncaughtException ex
#endif
#endif

handleUncaughtException :: SomeException -> T.Result
handleUncaughtException ex =
  T.testFailed ("uncaught exception: " ++ H.formatException ex)

-- | How to treat @hspec@ pending tests.
--
-- @tasty@ does not have the concept of pending tests, so we must map them to
-- either successes or failures. By default, they are treated as failures.
--
-- Set via the command line flag @--treat-pending-as (success|failure)@.
data TreatPendingAs
  = TreatPendingAsFailure
  | TreatPendingAsSuccess

instance T.IsOption TreatPendingAs where
  defaultValue =
    TreatPendingAsFailure

  parseValue s =
    case s of
      "failure" -> Just TreatPendingAsFailure
      "success" -> Just TreatPendingAsSuccess
      _         -> Nothing

  optionName =
    pure "treat-pending-as"

  optionHelp =
    pure "How to treat pending hspec tests ('failure' or 'success')"

#if MIN_VERSION_tasty(1,3,0)
  showDefaultValue _ =
    Just "failure"
#endif
