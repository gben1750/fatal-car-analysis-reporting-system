library(testthat)
library(fatalcaranalysiisreports)

context("Correct Number of records exist in the 2013 file")

test_that("Test number of records in 2013 dataset should equal 30202", {
  fileName = 'accident_2013.csv'
  tbl <- fars_read(fileName)
  expect_equal(nrow(tbl), 30202)
})

test_that("Test number of records in 2015 dataset should equal 30056", {
  fileName = 'accident_2014.csv'
  tbl <- fars_read(fileName)
  expect_equal(nrow(tbl), 30056)
})
