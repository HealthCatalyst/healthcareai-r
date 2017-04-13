# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) 
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

### Added
- Added getOutDf to each algorithm deploy file so predictions can go to CSV
- Added percentDataAvailableInDateRange, to eventually replace countPercentEmpty
- Added featureAvailabilityProfiler

### Changed
- TimeStamp column predictive output is now local time (not GMT)

### Fixed

## [0.1.11] - 2017-03-02

### Added

- Added changelog
- Added travis.yml to prepare for CRAN release

### Changed

- generateAUC now calls getCutOffs to give guidance on ideal cutoffs.
- getCutOffs now generates list of cutoffs and suggests ideal ones.
- API changes for both functions.
- calculatePerformance (model class method) now calls generateAUC

### Fixed
- Bug fixes in example files concerning reproducability

[Unreleased]: https://github.com/HealthCatalystSLC/healthcareai-r/compare/v0.1.11...HEAD
[0.1.11]: https://github.com/HealthCatalystSLC/healthcareai-r/releases/tag/v0.1.11-beta