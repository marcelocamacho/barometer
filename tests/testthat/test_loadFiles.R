context("import datasets tests")

test_that("loadDataFromFiles returns expected variables in GlobalEnv",{
 loadDataFromFiles()
 expect_match(list("censo","regAdm"), ls())
})

#test_that("make_filename returns expected filename when passing year as character",{
# expect_match("accident_2013.csv.bz2", make_filename("2013"))
#})

#test_that("make_filename returns a warning when passing an invalid character",{
# invalid_year = "a_string"
# expect_warning(make_filename(invalid_year), "NAs introduced by coercion")
