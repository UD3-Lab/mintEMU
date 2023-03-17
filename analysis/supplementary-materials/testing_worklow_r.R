# create testing directory
usethis::use_testthat()

# create a file for testing functions from clean.R script
usethis::use_test('clean')
# modify tests with assertion statements : https://testthat.r-lib.org/reference/index.html

# Run the tests
devtools::test()
