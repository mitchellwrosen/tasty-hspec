{-# LANGUAGE CPP #-}

-- | @hspec@ and @tasty@ serve similar purposes; consider using one or the
-- other.
--
-- However, in a pinch, this module allows you to run an @hspec@ 'H.Spec' as a
-- @tasty@ 'T.TestTree'.
module Test.Tasty.Hspec
  ( -- * Tests
    testSpec,
    testSpecs,

    -- * Options
    TreatPendingAs (..),

    -- * Examples
    -- $examples
  )
where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Proxy
import Data.Typeable (Typeable)
import qualified Test.Hspec as H
import qualified Test.Hspec.Core.Formatters as H
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Tasty as T
import Test.Tasty.Hspec.Compat
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Providers as T
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.Tasty.Runners as T
import qualified Test.Tasty.SmallCheck as TSC

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
--     [ unsafePerformIO (testSpec "My first Hspec test" spec_firstHspecTest)
--     , ...
--     ]
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
testSpec name spec = do
  trees <- testSpecs spec
  pure (T.testGroup name trees)

-- | Create a list of <https://hackage.haskell.org/package/tasty tasty> 'T.TestTree' from an
-- <https://hackage.haskell.org/package/hspec Hspec> 'H.Spec'. This returns the same tests as 'testSpec'
-- but doesn't create a <https://hackage.haskell.org/package/tasty tasty> test group from them.
testSpecs :: H.Spec -> IO [T.TestTree]
testSpecs spec = do
  -- Here we do as hspec does, which is pre-process a spec by focusing the whole thing, which is a no-op if
  -- anything inside is already focused, but otherwise focuses every item. Then, when creating a tasty test tree,
  -- we just toss the unfocused items.
  trees <- H.runSpecM (focus spec)
  pure (mapMaybe specTreeToTestTree trees)

specTreeToTestTree :: H.SpecTree () -> Maybe T.TestTree
specTreeToTestTree = \case
  Node name trees -> pure (T.testGroup name (mapMaybe specTreeToTestTree trees))
  NodeWithCleanup cleanup trees -> do
    tree <- specTreeToTestTree (H.Node "(unnamed)" trees)
    pure (T.WithResource (T.ResourceSpec (return ()) cleanup) (const tree))
  Leaf item -> do
    guard (itemIsFocused item)
    pure (T.singleTest (H.itemRequirement item) (Item item))

newtype Item
  = Item (H.Item ())
  deriving (Typeable)

instance T.IsTest Item where
  run opts (Item item) progress = do
    qcArgs <- optionSetToQuickCheckArgs opts
    H.Result _ result <- itemExample item (params qcArgs) ($ ()) progress'
    pure
      ( case result of
          H.Success -> T.testPassed ""
          H.Pending _ reason ->
            case T.lookupOption opts of
              TreatPendingAsFailure -> T.testFailed reason'
              TreatPendingAsSuccess -> T.testPassed reason'
            where
              reason' = "# PENDING: " ++ fromMaybe "No reason given" reason
          H.Failure _ reason ->
            case reason of
              H.NoReason -> T.testFailed ""
              H.Reason x -> T.testFailed x
              H.ExpectedButGot preface expected actual ->
                T.testFailed . unlines . catMaybes $
                  [ preface,
                    Just ("expected: " ++ expected),
                    Just (" but got: " ++ actual)
                  ]
              H.Error _ exception -> T.testFailed ("uncaught exception: " ++ H.formatException exception)
      )
    where
      params qcArgs =
        H.Params
          { H.paramsQuickCheckArgs = qcArgs,
            H.paramsSmallCheckDepth =
              case T.lookupOption opts of
                TSC.SmallCheckDepth depth ->
                  depth
          }

      progress' (x, y) =
        progress
          T.Progress
            { T.progressText = "",
              T.progressPercent = fromIntegral x / fromIntegral y
            }

  testOptions =
    pure
      [ T.Option (Proxy :: Proxy TreatPendingAs),
        T.Option (Proxy :: Proxy TQC.QuickCheckTests),
        T.Option (Proxy :: Proxy TQC.QuickCheckReplay),
        T.Option (Proxy :: Proxy TQC.QuickCheckMaxSize),
        T.Option (Proxy :: Proxy TQC.QuickCheckMaxRatio),
        T.Option (Proxy :: Proxy TSC.SmallCheckDepth)
      ]

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

  parseValue = \case
    "failure" -> Just TreatPendingAsFailure
    "success" -> Just TreatPendingAsSuccess
    _ -> Nothing

  optionName =
    pure "treat-pending-as"

  optionHelp =
    pure "How to treat pending hspec tests ('failure' or 'success')"

#if MIN_VERSION_tasty(1,3,0)
  showDefaultValue _ =
    Just "failure"
#endif
