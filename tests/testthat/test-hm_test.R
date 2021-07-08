context("hm_set")


# create an hydro-met station
guido <- hm_create(class_name = 'station')

# try to break the code arguments

test_that("obj bad entry", {
  expect_error(
    hm_set(obj = 'guido')
  )

  expect_error(
    hm_set(obj = gudo)
  )

  expect_error(
    hm_set(obj = TRUE)
  )


  expect_error(
    hm_set(obj = 1)
  )


})
#> Test passed


test_that("class bad entry", {
  expect_error(
    hm_set(obj = guido,
           agency = 123)
  )

  expect_error(
    hm_set(obj = guido,
           tair = mtcars)
  )

  expect_error(
    hm_set(obj = guido,
           tair = FALSE)
  )

  expect_error(
    hm_set(obj = guido,
           tair = 1:10)
  )

  expect_error(
    hm_set(obj = guido,
           tair = matrix(1:10, 2) )
  )


})
#> Test passed
