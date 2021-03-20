library(testthat)
library(fatalcaranalysiisreports)

context("Correct Number of records exist in the 2013 file")

test_that("Test number of records in 2013 dataset should equal 30202", {
  fileName = 'accident_2013.csv'
  tbl <- fars_read(fileName)
  expect_equal(nrow(tbl), 30202)
})
