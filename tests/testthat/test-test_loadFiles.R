test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("The files of REGISTROS ADMINISTRATIVOS is on data directory ",{

 fileExist = file.exists(
  file.path("..","..",
            "data",
            'DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx'
  ))

 expect_true(
  fileExist
 )
})

test_that("The files of CENSO is on data directory ",{

 fileExist = file.exists(
  file.path("..","..",
            "data",
            'Atlas 2013_municipal, estadual e Brasil.xlsx'
  )
 )

 expect_true(
  fileExist
 )

})
