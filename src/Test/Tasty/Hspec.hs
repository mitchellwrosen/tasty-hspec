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

import Control.Monad (guard)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Proxy
import Data.Typeable (Typeable)
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.Core.Formatters as Hspec.Core.Formatters
import qualified Test.Hspec.Core.Spec as Hspec.Core.Spec
import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec.Compat
import qualified Test.Tasty.Options as Tasty.Options
import qualified Test.Tasty.Providers as Tasty.Providers
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.Tasty.Runners as Tasty.Runners
import qualified Test.Tasty.SmallCheck as TSC

-- $examples
--
-- The simplest usage of this library involves first creating a 'T.TestTree' in @IO@, then running it with
-- 'T.defaultMain'.
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
-- If you don't do any @IO@ during 'Spec' creation, or the @IO@ need
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
-- <https://hackage.haskell.org/package/hspec hspec> 'H.Spec'.
testSpec :: Tasty.TestName -> Hspec.Spec -> IO Tasty.TestTree
testSpec name spec = do
  trees <- testSpecs spec
  pure (Tasty.testGroup name trees)

-- | Create a list of <https://hackage.haskell.org/package/tasty tasty> 'T.TestTree' from an
-- <https://hackage.haskell.org/package/hspec hspec> 'H.Spec'. This returns the same tests as 'testSpec'
-- but doesn't create a <https://hackage.haskell.org/package/tasty tasty> test group from them.
testSpecs :: Hspec.Spec -> IO [Tasty.TestTree]
testSpecs spec = do
  -- Here we do as hspec does, which is pre-process a spec by focusing the whole thing, which is a no-op if
  -- anything inside is already focused, but otherwise focuses every item. Then, when creating a tasty test tree,
  -- we just toss the unfocused items.
  trees <- runSpecM (Hspec.focus spec)
  pure (mapMaybe specTreeToTestTree trees)

specTreeToTestTree :: Hspec.Core.Spec.SpecTree () -> Maybe Tasty.TestTree
specTreeToTestTree = \case
  Node name trees -> pure (Tasty.testGroup name (mapMaybe specTreeToTestTree trees))
  NodeWithCleanup cleanup trees -> do
    tree <- specTreeToTestTree (Node "(unnamed)" trees)
    pure (Tasty.Runners.WithResource (Tasty.Runners.ResourceSpec (pure ()) (twiddleCleanup cleanup)) (const tree))
  Leaf item -> do
    guard (Hspec.Core.Spec.itemIsFocused item)
    pure (Tasty.Providers.singleTest (Hspec.Core.Spec.itemRequirement item) (Item item))

newtype Item
  = Item (Hspec.Core.Spec.Item ())
  deriving (Typeable)

instance Tasty.Providers.IsTest Item where
  run opts (Item item) progress = do
    qcArgs <- optionSetToQuickCheckArgs opts
    let params =
          Hspec.Core.Spec.Params
            { Hspec.Core.Spec.paramsQuickCheckArgs = qcArgs,
              Hspec.Core.Spec.paramsSmallCheckDepth = optionSetToSmallCheckDepth opts
            }
    Hspec.Core.Spec.Result _ result <- Hspec.Core.Spec.itemExample item params ($ ()) progress'
    pure
      ( case result of
          Hspec.Core.Spec.Success -> Tasty.Providers.testPassed ""
          Hspec.Core.Spec.Pending _ reason ->
            case Tasty.Options.lookupOption opts of
              TreatPendingAsFailure -> Tasty.Providers.testFailed reason'
              TreatPendingAsSuccess -> Tasty.Providers.testPassed reason'
            where
              reason' = "# PENDING: " ++ fromMaybe "No reason given" reason
          Hspec.Core.Spec.Failure _ reason ->
            case reason of
              Hspec.Core.Spec.NoReason -> Tasty.Providers.testFailed ""
              Hspec.Core.Spec.Reason x -> Tasty.Providers.testFailed x
              Hspec.Core.Spec.ExpectedButGot preface expected actual ->
                Tasty.Providers.testFailed . unlines . catMaybes $
                  [ preface,
                    Just ("expected: " ++ expected),
                    Just (" but got: " ++ actual)
                  ]
              Hspec.Core.Spec.Error _ exception ->
                Tasty.Providers.testFailed ("uncaught exception: " ++ Hspec.Core.Formatters.formatException exception)
      )
    where
      progress' (x, y) =
        progress
          Tasty.Runners.Progress
            { Tasty.Runners.progressText = "",
              Tasty.Runners.progressPercent = fromIntegral x / fromIntegral y
            }

  testOptions =
    pure
      [ Tasty.Options.Option (Proxy :: Proxy TreatPendingAs),
        Tasty.Options.Option (Proxy :: Proxy TQC.QuickCheckTests),
        Tasty.Options.Option (Proxy :: Proxy TQC.QuickCheckReplay),
        Tasty.Options.Option (Proxy :: Proxy TQC.QuickCheckMaxSize),
        Tasty.Options.Option (Proxy :: Proxy TQC.QuickCheckMaxRatio),
        Tasty.Options.Option (Proxy :: Proxy TSC.SmallCheckDepth)
      ]

-- | How to treat @hspec@ pending tests.
--
-- @tasty@ does not have the concept of pending tests, so we must map them to
-- either successes or failures. By default, they are treated as failures.
--
-- Set via the command line flag @--treat-pending-as (success|failure)@.
data TreatPendingAs
  = -- | Default.
    TreatPendingAsFailure
  | TreatPendingAsSuccess

instance Tasty.Options.IsOption TreatPendingAs where
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
