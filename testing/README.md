# This project is used for testing the CodeCadenza plug-ins.

## Prerequisites
In order to be able to run all tests on a new system there are some steps that must be done in advance:

1. Add the **settings-cc.xml** Maven configuration file to the **/{user.home}/.m2** folder.

2. Some tests require a local **MySQL** and a **PostgreSQL** database with a user **sa** and password
   **sa**. Note, that these user accounts need the permissions to create schemas, tables and sequences!
   As the project doesn't contain any JDBC libraries, a test searches for the respective driver in the
   **/{user.home}/development/lib/jdbc/{mysql | postgresql}** folder.

3. In order to run tests that generate Eclipse RAP applications it is necessary that these tests add an
   Eclipse RAP runtime, which must be available in the **/{user.home}/development/eclipse/rap** folder.
