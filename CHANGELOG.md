# Changelog

## Unreleased

- Remove support for `hspec < 2.10`
- Remove support for `hspec-core < 2.10`

## [1.2.0.2] - 2023-01-10

- Fix build when `tasty-quickcheck < 0.9.1`

## [1.2.0.1] - 2022-05-14

- Support hspec-2.10.0

## [1.2] - 2021-05-28

- No changes, but 1.1.7 should have been a major version bump due to dropping the `Test.Hspec` re-export.

## [1.1.7] - 2021-05-12

- Support hspec-2.8.0
- Remove re-export of `Test.Hspec`
- Drop support for hspec < 2.5

## [1.1.6-r1] - 2021-03-13

- Support base-4.15

## [1.1.6] - 2020-11-11

- Rename `Success`/`Failure` to `TreatPendingAsSuccess`/`TreatPendingAsFailure`
- Support tasty-1.4

## [1.1.5.1] - 2018-11-15

- Support hspec-2.6.0

## [1.1.5] - 2018-06-27

- Add `TreatPendingAs` option, for allowing pending tests to be treated as successes

## [1.1.4] - 2018-3-18

- Support hspec-2.5.0
- Remove re-exports of QuickCheck and SmallCheck options

## [1.1.3.3] - 2018-1-26

- Remove redundant `random` dependency

## [1.1.3.2] - 2017-6-20

- Support tasty-0.9.1
- More accurate dependency version bounds

## [1.1.3.1] - 2017-1-25

- Support hspec-2.4.0
