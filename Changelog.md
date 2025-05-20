# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Fixed
2025-05-20: Fix generation of wrong plural label when creating a new domain attribute
### Added
2025-05-20: Add a label for the total number of elements to the element collection editors of all supported client technologies
### Added
2025-05-15: Add the filtering of elements displayed in the element collection editors of all supported client technologies
### Fixed
2025-05-15: Disable fields in the property sheet that must not be changed for attributes that are mapped to an element collection
### Changed
2025-05-14: Add a check to prevent the duplicate initialization of an element collection attribute during a subsequent reverse
            engineering process
### Changed
2025-05-11: Disable the field that controls if a domain attribute is nullable for element collections in the respective reverse
            engineering dialog
### Fixed
2025-05-11: A warning message dialog won't be opened if a value cannot be added to an element collection in a JSF application
### Changed
2025-05-09: Improve the initialization of domain model enumerations by performing a cleanup
### Changed
2025-05-09: Use isEmpty() to check whether a string is empty or not
### Fixed
2025-05-09: Change formatting of Javadoc comments by performing a cleanup
### Fixed
2025-05-09: Fix the broken ValueConverter test, which fails if a locale other than 'EN' is used
### Fixed
2025-05-09: Fix wrong ordering of import statements
### Changed
2025-05-08: Initialize new domain attributes as non-nullable when using the 'CONVERTER' strategy
### Fixed
2025-05-08: Move the ValueConverterTest to the correct package
### Fixed
2025-05-08: Use a fixed grouping separator and a fixed decimal separator for saving decimal values of an element collection that
            use the 'CONVERTER' strategy in a locale-independent format
### Fixed
2025-05-07: Fix formatting in plug-ins and runtime libraries to adhere to the maximum line width
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
