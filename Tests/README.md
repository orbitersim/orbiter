# Orbiter integration and unit tests

## Unit tests

Unit tests are written as .cpp files in this directory and registered using `add_test_file` CMake function. Naming convention is "Module.Test.cpp"

## Integration tests

Integration tests are implemented by

1. Adding a Lua script to Script\Tests directory
1. Adding a corresponding scenario which invokes this script to Scenarios\Tests directory

Lua script must:

1. Ensure test runs for limited time
1. Call oapi.exit(code) when test is finished
1. Pass non-zero return code in case of failed test, and 0 - if all tests are successful
1. Print information about execution using oapi.write_log