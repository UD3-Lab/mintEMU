# Resources:
# https://www.pipinghotdata.com/posts/2021-11-23-getting-started-with-unit-testing-in-r/
# https://carpentries-incubator.github.io/lesson-R-packaging/06-testing/index.html
# https://rklopotek.blog.uksw.edu.pl/files/2017/09/package-development.pdf


# create testing directory
usethis::use_testthat()

# create a file for testing functions from clean.R script
usethis::use_test('clean')
# modify tests with assertion statements : https://testthat.r-lib.org/reference/index.html

# Run the tests
devtools::test()
