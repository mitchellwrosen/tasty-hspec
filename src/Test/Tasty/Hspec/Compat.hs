{-# LANGUAGE CPP #-}

module Test.Tasty.Hspec.Compat
  ( optionSetToQuickCheckArgs,
  )
where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

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
    Tasty.QuickCheck.QuickCheckTests num_tests = Tasty.lookupOption opts
    Tasty.QuickCheck.QuickCheckReplay replay = Tasty.lookupOption opts
    Tasty.QuickCheck.QuickCheckMaxSize max_size = Tasty.lookupOption opts
    Tasty.QuickCheck.QuickCheckMaxRatio max_ratio = Tasty.lookupOption opts
#endif
