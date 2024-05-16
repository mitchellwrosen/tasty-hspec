## [1.2.0.4] - April 25, 2023

- Add support for `hspec-2.11`
- Drop support for `hspec-2.10`
- Drop support for GHC < 8.4

## [1.2.0.3] - January 11, 2023

- Drop support for `hspec < 2.10`, `hspec-core < 2.10`, `tasty < 1.3`, and `tasty-quickcheck < 0.9.1`. This was done to
  eliminate CPP and reduce maintenance burden. Moving forward, this library will opt to tighten compatibility bounds
  when dependencies make breaking changes, rather than reintroduce CPP.

## [1.2.0.2] - January 10, 2023

- Fix build when `tasty-quickcheck < 0.9.1`

## [1.2.0.1] - May 14, 2022

- Support hspec-2.10.0

## [1.2] - May 28, 2021

- No changes, but 1.1.7 should have been a major version bump due to dropping the `Test.Hspec` re-export.

## [1.1.7] - May 12, 2021

- Support hspec-2.8.0
- Remove re-export of `Test.Hspec`
- Drop support for hspec < 2.5

## [1.1.6-r1] - March 13, 2021

- Support base-4.15

## [1.1.6] - November 11, 2020

- Rename `Success`/`Failure` to `TreatPendingAsSuccess`/`TreatPendingAsFailure`
- Support tasty-1.4

## [1.1.5.1] - November 15, 2018

- Support hspec-2.6.0

## [1.1.5] - June 27, 2018

- Add `TreatPendingAs` option, for allowing pending tests to be treated as successes

## [1.1.4] - March 18, 2018

- Support hspec-2.5.0
- Remove re-exports of QuickCheck and SmallCheck options

## [1.1.3.3] - January 26, 2018

- Remove redundant `random` dependency

## [1.1.3.2] - June 20, 2017

- Support tasty-0.9.1
- More accurate dependency version bounds

## [1.1.3.1] - January 25, 2017

- Support hspec-2.4.0
