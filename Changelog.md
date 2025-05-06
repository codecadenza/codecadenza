# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Fixed
2025-05-06: Replace invalid characters in the translation files of different runtime libraries
### Changed
2025-05-06: Use the XLSX export format instead of XLS in JSF data tables
### Added
2025-05-05: Add support for element collections
### Changed
2025-05-05: Add double quotes to all fields when exporting the content of the SQL query editor to a csv file
### Added
2025-05-05: Add a tool for changing the timestamps of all entries contained in a zip file
### Changed
2025-05-02: Improve the layout of the dialog for maintaining table column data
### Fixed
2025-05-02: Add missing checks to avoid NullPointerExceptions in data exchange generators
### Changed
2025-02-25: Use a method reference instead of a lambda expression to configure CSRF in generated Spring Boot applications
### Fixed
2025-02-24: Ensure that the input stream of an uploaded file is closed in a generated JSF application prior to further processing

## 1.0.0 - 2025-02-01
### Added
- Initial release
