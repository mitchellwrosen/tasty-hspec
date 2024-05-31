-- | @<https://hackage.haskell.org/package/hspec hspec>@ and @<https://hackage.haskell.org/package/tasty tasty>@ serve
-- similar purposes; consider using one or the other.
--
-- However, in a pinch, this module allows you to run an @<https://hackage.haskell.org/package/hspec hspec>@
-- 'Hspec.Spec' as a @<https://hackage.haskell.org/package/tasty tasty>@ 'Tasty.TestTree'.
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
import qualified Test.Hspec.Api.Formatters.V1 as Hspec.Api.Formatters.V1
import qualified Test.Hspec.Core.Spec as Hspec.Core.Spec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Options as Tasty.Options
import qualified Test.Tasty.Providers as Tasty.Providers
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck
import qualified Test.Tasty.Runners as Tasty.Runners
import qualified Test.Tasty.SmallCheck as Tasty.SmallCheck

-- $examples
--
-- The simplest usage of this library involves first creating a 'Tasty.TestTree' in @IO@, then running it with
-- 'Tasty.defaultMain'.
--
-- @
-- main = do
--   spec <- 'testSpec' "spec" mySpec
--   'Tasty.defaultMain' ('Tasty.testGroup' "tests" [spec])
-- @
--
-- You can treat an 'Hspec.pending'/'Hspec.pendingWith' test as a success instead of a failure (the default):
--
-- @
-- main = do
--   spec <- 'testSpec' "spec" mySpec
--   'Tasty.defaultMain' ('Tasty.localOption' 'TreatPendingAsSuccess' ('Tasty.testGroup' "tests" [spec]))
-- @
--
-- If you don't do any @IO@ during 'Hspec.Spec' creation, or the @IO@ need
-- not be performed at any particular time relative to other @IO@ actions, it's
-- perfectly fine to use 'System.IO.unsafePerformIO'.
--
-- @
-- main = do
--   'Tasty.defaultMain' ('Tasty.testGroup' "tests" [unsafePerformIO ('testSpec' "spec" mySpec)])
-- @

-- | Create a @<https://hackage.haskell.org/package/tasty tasty>@ 'Tasty.TestTree' from an
-- @<https://hackage.haskell.org/package/hspec hspec>@ 'Hspec.Spec'.
testSpec :: Tasty.TestName -> Hspec.Spec -> IO Tasty.TestTree
testSpec name spec = do
  trees <- testSpecs spec
  pure (Tasty.testGroup name trees)

-- | Create a list of @<https://hackage.haskell.org/package/tasty tasty>@ 'Tasty.TestTree' from an
-- @<https://hackage.haskell.org/package/hspec hspec>@ 'Hspec.Spec'. This returns the same tests as 'testSpec', but
-- doesn't create a @<https://hackage.haskell.org/package/tasty tasty>@ test group from them.
testSpecs :: Hspec.Spec -> IO [Tasty.TestTree]
testSpecs spec = do
  -- Here we do as hspec does, which is pre-process a spec by focusing the whole thing, which is a no-op if
  -- anything inside is already focused, but otherwise focuses every item. Then, when creating a tasty test tree,
  -- we just toss the unfocused items.
  (_configBuilder, trees) <- Hspec.Core.Spec.runSpecM (Hspec.focus spec)
  pure (mapMaybe specTreeToTestTree trees)

specTreeToTestTree :: Hspec.Core.Spec.SpecTree () -> Maybe Tasty.TestTree
specTreeToTestTree = \case
  Hspec.Core.Spec.Node name trees -> pure (Tasty.testGroup name (mapMaybe specTreeToTestTree trees))
  Hspec.Core.Spec.NodeWithCleanup _loc cleanup trees -> do
    tree <- specTreeToTestTree (Hspec.Core.Spec.Node "(unnamed)" trees)
    pure (Tasty.Runners.WithResource (Tasty.Runners.ResourceSpec (pure ()) (const cleanup)) (const tree))
  Hspec.Core.Spec.Leaf item -> do
    guard (Hspec.Core.Spec.itemIsFocused item)
    pure (Tasty.Providers.singleTest (Hspec.Core.Spec.itemRequirement item) (Item item))

newtype Item
  = Item (Hspec.Core.Spec.Item ())
  deriving (Typeable)

instance Tasty.Providers.IsTest Item where
  run opts (Item item) progress = do
    (_, qcArgs) <- Tasty.QuickCheck.optionSetToArgs opts
    -- optionSetToQuickCheckArgs :: Tasty.OptionSet -> IO QuickCheck.Args
    -- optionSetToQuickCheckArgs opts =
    --   snd <$> Tasty.QuickCheck.optionSetToArgs opts
    let params =
          Hspec.Core.Spec.Params
            { Hspec.Core.Spec.paramsQuickCheckArgs = qcArgs,
              Hspec.Core.Spec.paramsSmallCheckDepth =
                case Tasty.Options.lookupOption opts of
                  Tasty.SmallCheck.SmallCheckDepth depth -> Just depth
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
              Hspec.Core.Spec.ColorizedReason x -> Tasty.Providers.testFailed x
              Hspec.Core.Spec.NoReason -> Tasty.Providers.testFailed ""
              Hspec.Core.Spec.Reason x -> Tasty.Providers.testFailed x
              Hspec.Core.Spec.ExpectedButGot preface expected actual ->
                Tasty.Providers.testFailed . unlines . catMaybes $
                  [ preface,
                    Just ("expected: " ++ expected),
                    Just (" but got: " ++ actual)
                  ]
              Hspec.Core.Spec.Error _ exception ->
                Tasty.Providers.testFailed ("uncaught exception: " ++ Hspec.Api.Formatters.V1.formatException exception)
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
        Tasty.Options.Option (Proxy :: Proxy Tasty.QuickCheck.QuickCheckTests),
        Tasty.Options.Option (Proxy :: Proxy Tasty.QuickCheck.QuickCheckReplay),
        Tasty.Options.Option (Proxy :: Proxy Tasty.QuickCheck.QuickCheckMaxSize),
        Tasty.Options.Option (Proxy :: Proxy Tasty.QuickCheck.QuickCheckMaxRatio),
        Tasty.Options.Option (Proxy :: Proxy Tasty.SmallCheck.SmallCheckDepth)
      ]

-- | How to treat @<https://hackage.haskell.org/package/hspec hspec>@ pending tests.
--
-- @<https://hackage.haskell.org/package/tasty tasty>@ does not have the concept of pending tests, so we must map them
-- to either successes or failures. By default, they are treated as failures.
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

  showDefaultValue _ =
    Just "failure"
