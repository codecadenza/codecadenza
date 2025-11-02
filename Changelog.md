# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
2025-10-21: Log the generated JPA query parameters and their values
2025-10-19: Add support for converting values with primitive types in the ValueConverter class

### Changed
2025-11-02: Do not return a value if a generated Kafka integration method has no response schema
2025-10-31: Make list DTO attributes optional if they are not used for setting the reference in generated Avro IDL files
2025-10-28: Improve handling of optional parameter filter values in generated unique key methods
2025-10-26: Initialize unique key repository method parameters that are mapped to an association using the type of the primary key
            instead of the domain object
2025-10-22: Escape XML characters by using the Apache commons text library when generating test data files for GUI tests
2025-10-21: Correct a wrong local variable name in the menu builder for GUI test cases
2025-10-21: Declare date attributes optional that are automatically set when creating a new object in the generated Kafka IDL file
2025-10-19: Make the Avro fields 'operator' and 'sortOrder' optional for the SearchInput record, set appropriate default values
            and use the question mark instead of a union for optional attributes
2025-10-19: Disable precision and scale for new domain attributes that are not mapped to BigDecimal

### Fixed
2025-10-26: Avoid exceptions when launching Java classes and unit tests by considering the name of the class and the project name
            when searching for existing launch configurations
2025-10-21: Fix the wrong initialization of the media type for a new REST integration method that performs a data export operation
2025-10-21: Fix the wrong layout of the integration wizard page
2025-10-19: Fix the wrong return value in generated Kafka client methods if they don't have a response schema
2025-10-19: Set the 'initialized' flag when setting up the JMS file service client
2025-10-17: Fix typos in Javadoc comments

## 1.1.0 - 2025-05-24

### Added
2025-05-20: Add a label for the total number of elements to the element collection editors of all supported client technologies
2025-05-15: Add the filtering of elements displayed in the element collection editors of all supported client technologies
2025-05-05: Add support for element collections
2025-05-05: Add a tool for changing the timestamps of all entries contained in a zip file

### Changed
2025-05-14: Add a check to prevent the duplicate initialization of an element collection attribute during a subsequent reverse
            engineering process
2025-05-11: Disable the field that controls if a domain attribute is nullable for element collections in the respective reverse
            engineering dialog
2025-05-09: Improve the initialization of domain model enumerations by performing a cleanup
2025-05-09: Use isEmpty() to check whether a string is empty or not
2025-05-08: Initialize new domain attributes as non-nullable when using the 'CONVERTER' strategy
2025-05-06: Use the XLSX export format instead of XLS in JSF data tables
2025-05-05: Add double quotes to all fields when exporting the content of the SQL query editor to a csv file
2025-05-02: Improve the layout of the dialog for maintaining table column data
2025-02-25: Use a method reference instead of a lambda expression to configure CSRF in generated Spring Boot applications

### Fixed
2025-05-23: Save the translations for enum literals when creating either a grid panel, a tree view or a view form
2025-05-21: Avoid adding DTO attributes that are mapped to transient fields, byte arrays, or element collections if the DTO is
            intended for search operations
2025-05-20: Avoid adding attributes to a DTO that are mapped to transient fields, byte arrays, or element collections if the DTO
            is intended for search operations
2025-05-20: Fix generation of wrong plural label when creating a new domain attribute
2025-05-15: Disable fields in the property sheet that must not be changed for attributes that are mapped to an element collection
2025-05-11: A warning message dialog won't be opened if a value cannot be added to an element collection in a JSF application
2025-05-09: Change formatting of Javadoc comments by performing a cleanup
2025-05-09: Fix the broken ValueConverter test, which fails if a locale other than 'EN' is used
2025-05-09: Fix wrong ordering of import statements
2025-05-08: Move the ValueConverterTest to the correct package
2025-05-08: Use a fixed grouping separator and a fixed decimal separator for saving decimal values of an element collection that
            use the 'CONVERTER' strategy in a locale-independent format
2025-05-07: Fix formatting in plug-ins and runtime libraries to adhere to the maximum line width
2025-02-24: Ensure that the input stream of an uploaded file is closed in a generated JSF application prior to further processing
2025-05-02: Add missing checks to avoid NullPointerExceptions in data exchange generators
2025-05-06: Replace invalid characters in the translation files of different runtime libraries

## 1.0.0 - 2025-02-01

### Added
- Initial release
