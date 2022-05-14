{-# LANGUAGE CPP #-}

module Test.Tasty.Hspec.Compat
  ( itemExample,
    itemIsFocused,
    focus,
    optionSetToQuickCheckArgs,
    optionSetToSmallCheckDepth,
    runSpecM,
    twiddleCleanup,

    pattern Leaf,
    pattern Node,
    pattern NodeWithCleanup,
  )
where

import qualified Test.Hspec as Hspec
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as Tasty.SmallCheck
#if MIN_VERSION_hspec_core(2,10,0)
import Data.Monoid (Endo)
import qualified Test.Hspec.Core.Runner as Hspec.Core.Runner
#endif

{-# COMPLETE Leaf, Node, NodeWithCleanup #-}

pattern Leaf :: a -> Hspec.Tree c a
pattern Leaf item <-
  Hspec.Leaf item

pattern Node :: String -> [Hspec.Tree c a] -> Hspec.Tree c a
pattern Node name trees <-
  Hspec.Node name trees

pattern NodeWithCleanup :: c -> [Hspec.Tree c a] -> Hspec.Tree c a
pattern NodeWithCleanup cleanup trees <-
#if MIN_VERSION_hspec(2,8,0)
  Hspec.NodeWithCleanup _loc cleanup trees
#else
  Hspec.NodeWithCleanup cleanup trees
#endif

itemExample :: Hspec.Item a -> Hspec.Params -> (Hspec.ActionWith a -> IO ()) -> Hspec.ProgressCallback -> IO Hspec.Result
itemExample item =
  case item of
#if MIN_VERSION_hspec(2,6,0)
    Hspec.Item _ _ _ _ example -> example
#else
    Hspec.Item _ _ _ example -> example
#endif

itemIsFocused :: Hspec.Item a -> Bool
itemIsFocused =
#if MIN_VERSION_hspec(2,6,0)
  Hspec.itemIsFocused
#else
  const True
#endif

focus :: Hspec.Spec -> Hspec.Spec
focus =
#if MIN_VERSION_hspec(2,6,0)
  Hspec.focus
#else
  id
#endif

optionSetToQuickCheckArgs :: Tasty.OptionSet -> IO QuickCheck.Args
optionSetToQuickCheckArgs opts =
#if MIN_VERSION_tasty_quickcheck(0,9,1)
  snd <$> Tasty.QuickCheck.optionSetToArgs opts
#else
  pure
    QuickCheck.stdArgs
      { QuickCheck.chatty = False,
        QuickCheck.maxDiscardRatio = max_ratio,
        QuickCheck.maxSize = max_size,
        QuickCheck.maxSuccess = num_tests,
        QuickCheck.replay = replay
      }
  where
    Tasty.QuickCheck.QuickCheckTests num_tests = T.lookupOption opts
    Tasty.QuickCheck.QuickCheckReplay replay = T.lookupOption opts
    Tasty.QuickCheck.QuickCheckMaxSize max_size = T.lookupOption opts
    Tasty.QuickCheck.QuickCheckMaxRatio max_ratio = T.lookupOption opts
#endif

-- In hspec-core-2.10.0, Int changed to Maybe Int
optionSetToSmallCheckDepth ::
  Tasty.OptionSet ->
#if MIN_VERSION_hspec_core(2,10,0)
  Maybe
#endif
  Int
optionSetToSmallCheckDepth opts =
  case Tasty.lookupOption opts of
    Tasty.SmallCheck.SmallCheckDepth depth ->
#if MIN_VERSION_hspec_core(2,10,0)
      Just
#endif
      depth

-- In hspec-core-2.10.0, runSpecM started returning an Endo Config, which we don't need. (Right? :shrug:)
runSpecM :: Hspec.SpecWith a -> IO [Hspec.SpecTree a]
runSpecM spec = do
#if MIN_VERSION_hspec_core(2,10,0)
  (_ :: Endo Hspec.Core.Runner.Config, trees) <- Hspec.runSpecM spec
  pure trees
#else
  Hspec.runSpecM spec
#endif

-- In hspec-core-2.10.0, the spec SpecTree type alias changed from
--
--   type SpecTree a = Tree (a -> IO ()) (Item a)
--
-- to
--
--   type SpecTree a = Tree (IO ()) (Item a)
--
-- So we have a function that "twiddles" the cleanup action (at monomorphic type `SpecTree ()`), which always returns a
-- value of type `() -> IO ()`
#if MIN_VERSION_hspec_core(2,10,0)
twiddleCleanup :: IO () -> () -> IO ()
twiddleCleanup =
  const
#else
twiddleCleanup :: (() -> IO ()) -> () -> IO ()
twiddleCleanup =
  id
#endif
