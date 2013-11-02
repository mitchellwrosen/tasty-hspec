{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- | Hspec options, with default values and option descriptions last grabbed
-- from Test.Hspec.Runner in hspec-1.7.2.1
module Test.Tasty.Hspec.Options where

import Data.Data          (Data)
import Data.Ix            (Ix)
import Data.Generics      (Generic)
import Data.Typeable      (Typeable)
import Foreign.Storable   (Storable)
import Test.Hspec.Runner  (Path)
import Test.Tasty.Options (IsOption(..), safeRead)

-- Dry run

newtype HspecDryRun = HspecDryRun Bool
    deriving (Bounded, Eq, Data, Ord, Read, Show, Ix, Typeable)

instance Enum HspecDryRun where
    toEnum = HspecDryRun . toEnum
    fromEnum (HspecDryRun b) = fromEnum b

instance IsOption HspecDryRun where
    defaultValue   = HspecDryRun False
    parseValue     = fmap HspecDryRun . safeRead
    optionName     = return "hspec-dry-run"
    optionHelp     = return ""

-- Print CPU time

newtype HspecPrintCpuTime = HspecPrintCpuTime Bool
    deriving (Bounded, Eq, Data, Ord, Read, Show, Ix, Typeable)

instance Enum HspecPrintCpuTime where
    toEnum = HspecPrintCpuTime . toEnum
    fromEnum (HspecPrintCpuTime b) = fromEnum b

instance IsOption HspecPrintCpuTime where
    defaultValue   = HspecPrintCpuTime False
    parseValue     = fmap HspecPrintCpuTime . safeRead
    optionName     = return "hspec-print-cpu-time"
    optionHelp     = return ""

-- Fast fail

newtype HspecFastFail = HspecFastFail Bool
    deriving (Bounded, Eq, Data, Ord, Read, Show, Ix, Typeable)

instance Enum HspecFastFail where
    toEnum = HspecFastFail . toEnum
    fromEnum (HspecFastFail b) = fromEnum b

instance IsOption HspecFastFail where
    defaultValue   = HspecFastFail False
    parseValue     = fmap HspecFastFail . safeRead
    optionName     = return "hspec-fast-fail"
    optionHelp     = return ""

-- Filter predicate

newtype HspecFilterPredicate = HspecFilterPredicate (Maybe (Path -> Bool))
    deriving Typeable

instance IsOption HspecFilterPredicate where
    defaultValue   = HspecFilterPredicate Nothing
    parseValue     = undefined -- fmap HspecFastFail . safeRead
    optionName     = return "hspec-filter-predicate"
    optionHelp     = return "A predicate that is used to filter the spec before it is run. Only examples that satisfy the predicate are run."


