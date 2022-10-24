#context("fill_table")

# let's use a synthetic example to illustrate the use of the function
dates <- seq.Date(from = as.Date('1980-01-01'),
                  to = as.Date('2020-01-01'), by = 'day' )
var   <- runif(n = length(dates), min = 0, max = 100)

met_var <- data.frame(date = dates, random = var)[-c(50:100, 251, 38) , ]

# first try to break the code arguments

test_that("x bad entry", {
  expect_error( fill_table(x = TRUE) )
  expect_error( fill_table(x = as.matrix(met_var)) )
  expect_error( fill_table(x = 1:10) )
  expect_error( fill_table(x = 1L) )
})
#> Test passed

test_that("col_name bad entry", {
  expect_error( fill_table(x = met_var, col_name = "hi") )
  expect_error( fill_table(x = met_var, col_name = c("hi", "random") ) )
  expect_error( fill_table(x = met_var, col_name = 10) )
})
#> Test passed

test_that("by bad entry", {
  expect_error( fill_table(x = met_var, col_name = "random") )
  expect_error( fill_table(x = met_var, col_name = "random", by = '1') )
  expect_error( fill_table(x = met_var, col_name = "random", by = TRUE) )
})
#> Test passed

