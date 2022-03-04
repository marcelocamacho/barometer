test_that("External data files exists", {
  library(barometer)
  library(readxl)
  censo<-system.file("extdata",
                     "Atlas_2013_Censo_municipal_estadual_Brasil.xlsx",package = "barometer")
  regAdm<-system.file("extdata",
                      "Atlas_2013_RegistrosAdministrativos.xlsx", package = "barometer")
  testthat::expect_true(file.exists(censo))
  testthat::expect_true(file.exists(regAdm))
})

test_that("Load external data files", {

 TestLoadVarsfromPackageFunction <- loadDataFromFiles()
 result<- exists("TestLoadVarsfromPackageFunction")

 testthat::expect_true(result)

})
