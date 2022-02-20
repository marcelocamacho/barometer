

context("import datasets tests")

test_that("The files of REGISTROS ADMINISTRATIVOS is on data directory ",{
 fileExist = file.exists(
  'data/registrosAdministrativos/DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx'
 )

 cat(fileExist)

 expect_true(
  fileExist
  )
})

test_that("The files of CENSO is on data directory ",{

 fileExist = file.exists(
  'data/registrosCenso/Atlas 2013_municipal, estadual e Brasil.xlsx'
 )

 cat(fileExist)

expect_true(
  fileExist
  )

})
