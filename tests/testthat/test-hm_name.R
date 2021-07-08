context("hm_name")

# path to all example files
path <- system.file('extdata', package = 'hydrotoolbox')

# we first build the snih station file
guido <-
  hm_create() %>%
  hm_build(bureau = 'snih', path = path,
           file_name = c('snih_hq_guido.xlsx',
                         'snih_qd_guido.xlsx'),
           slot_name = c('hq', 'qd'),
           by = c('none', 'day') )

# try to break the code arguments
test_that("obj bad entry", {
  expect_error(
    hm_name(obj = TRUE,
            slot_name = 'qd',
            col_name = 'q(m3/s)')
    )

  expect_error(
    hm_name(obj = 1:10,
            slot_name = 'qd',
            col_name = 'q(m3/s)')
  )

  expect_error(
    hm_name(obj = 'guido',
            slot_name = 'qd',
            col_name = 'q(m3/s)')
  )

})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_name(obj = guido,
            slot_name = 'guido',
            col_name = 'q(m3/s)')
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = TRUE,
            col_name = 'q(m3/s)')
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = c('qd', 'hola'),
            col_name = 'q(m3/s)')
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = c('qd', 'swe'),
            col_name = 'q(m3/s)')
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = c('swe'),
            col_name = 'q(m3/s)')
  )

})
#> Test passed


test_that("col_name bad entry", {
  expect_error(
    hm_name(obj = guido,
            slot_name = 'qd',
            col_name = TRUE)
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = 'qd',
            col_name = 1:10)
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = 'qd',
            col_name = guido)
  )

  expect_error(
    hm_name(obj = guido,
            slot_name = 'qd',
            col_name = c('hi', 'bye'))
  )



})
#> Test passed


