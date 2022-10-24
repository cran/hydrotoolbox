#context("cum_sum")

# set path to file
path <- system.file('extdata', 'ianigla_cuevas.csv',
                    package = 'hydrotoolbox')

# read the file and add the new column with cumulative precipitation
cuevas <-
  read_ianigla(path = path)


# first try to break the code arguments

test_that("x bad arguments", {
  expect_error( cum_sum(x = 1:10) )
  expect_error( cum_sum(x = matrix(nrow = 10, ncol = 10)) )
  expect_error( cum_sum(x = FALSE ) )
})
#> Test passed

test_that("col_name bad arguments", {
  expect_error( cum_sum(x = cuevas, col_name = 2) )
  expect_error( cum_sum(x = cuevas, col_name = "any") )
  expect_error( cum_sum(x = FALSE ) )
})
#> Test passed

test_that("col_name bad arguments", {
  expect_error( cum_sum(x = cuevas, col_name = 'Precip_Total', out_name = 1) )
  expect_error( cum_sum(x = cuevas, col_name = 'Precip_Total', out_name = c("hi", "by")) )
  expect_error( cum_sum(x = cuevas, col_name = 'Precip_Total', out_name = TRUE) )
})
#> Test passed

test_that("col_name working", {
  expect_s3_class( cum_sum(x = cuevas, col_name = 'Precip_Total'),
                   class = "data.frame" )
  expect_s3_class( cum_sum(x = cuevas, col_name = 'Precip_Total', out_name = 'p_cum'),
                   class = "data.frame" )
})
#> Test passed
