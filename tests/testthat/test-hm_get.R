#context("hm_get")

my_hm <- hm_create()

# try to break the code arguments
test_that("obj bad entry", {
  expect_error( hm_get(obj = TRUE) )
  expect_error( hm_get(obj = 1:10) )
  expect_error( hm_get(obj = 'hola') )
  expect_error( hm_get(obj = list(hm_create(), hm_create())) )
})
#> Test passed

test_that("slot_name bad entry", {
  expect_error( hm_get(obj = my_hm, slot_name = TRUE) )
  expect_error( hm_get(obj = my_hm, slot_name = 1:10) )
  expect_error( hm_get(obj = my_hm, slot_name = 1) )
  expect_error( hm_get(obj = my_hm, slot_name = 'hola') )
})
#> Test passed
