{-# LANGUAGE CPP #-}

module Test.Tasty.Hspec.Compat
  ( itemExample,
    itemIsFocused,
    focus,
    optionSetToQuickCheckArgs,

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

