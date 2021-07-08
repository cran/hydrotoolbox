context("hm_create")

# try to break the code arguments
test_that("class_name bad entry", {
  expect_error( hm_create(class_name = TRUE) )
  expect_error( hm_create(class_name = 1:10) )
  expect_error( hm_create(class_name = NA_character_) )
  expect_error( hm_create(class_name = c('station', 'compact')) )
})
#> Test passed
