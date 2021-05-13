# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.1.7] - 2021-05-12

### Changed
- Support hspec-2.8.0

### Removed
- Don't re-export `Test.Hspec`
- Drop support for hspec < 2.5

## [1.1.6-r1] - 2021-03-13

### Changed
- Support base-4.15

## [1.1.6] - 2020-11-11

### Changed
- Rename `Success`/`Failure` to `TreatPendingAsSuccess`/`TreatPendingAsFailure`
- Support tasty-1.4

## [1.1.5.1] - 2018-11-15

### Changed
- Support hspec-2.6.0

## [1.1.5] - 2018-06-27

### Added
- `TreatPendingAs` option, for allowing pending tests to be treated as successes

## [1.1.4] - 2018-3-18

### Changed
- Support hspec-2.5.0

### Removed
- Re-exports of QuickCheck and SmallCheck options

## [1.1.3.3] - 2018-1-26

### Removed
- Redundant `random` dependency

## [1.1.3.2] - 2017-6-20

### Changed
- Support tasty-0.9.1
- More accurate dependency version bounds

## [1.1.3.1] - 2017-1-25

### Changed
- Support hspec-2.4.0
