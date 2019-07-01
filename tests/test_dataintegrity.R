test_that("read data correctly", {
  testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
})
