#context("hm_mutate")

# path to all example files
path <- system.file('extdata', package = 'hydrotoolbox')

# build the snih station file
guido <-
  hm_create() %>%
  hm_build(bureau = 'snih', path = path,
           file_name = c('snih_hq_guido.xlsx',
                         'snih_qd_guido.xlsx'),
           slot_name = c('hq', 'qd'),
           by = c('none', 'day') ) %>%
  hm_name(slot_name = 'qd',
          col_name = 'q(m3/s)')


# try to break the code arguments
test_that("obj bad entry", {
  expect_error(
    hm_mutate(obj = TRUE,
              slot_name = 'qd',
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

  expect_error(
    hm_mutate(obj = 1:10,
              slot_name = 'qd',
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

  expect_error(
    hm_mutate(obj = 'guido',
              slot_name = 'qd',
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )


})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_mutate(obj = guido,
              slot_name = TRUE,
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

  expect_error(
    hm_mutate(obj = guido,
              slot_name = c('qd', 'hw'),
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

  expect_error(
    hm_mutate(obj = guido,
              slot_name = 1:10,
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

  expect_error(
    hm_mutate(obj = guido,
              slot_name = list('qd'),
              FUN = mov_avg,
              k = 10, pos = 'c',
              out_name = 'mov_avg')
  )

})
#> Test passed


test_that("slot_name bad entry", {

  expect_error(
    suppressWarnings(
    hm_mutate(obj = guido,
              slot_name = 'qd',
              FUN = mean )
    )
  )


  expect_error(
    hm_mutate(obj = guido,
              slot_name = 'qd',
              FUN = sd)
  )

  expect_error(
    hm_mutate(obj = guido,
              slot_name = 'qd',
              FUN = function(x){as.matrix(x)})
  )

  expect_error(
    hm_mutate(obj = guido,
              slot_name = 'qd',
              FUN = function(x){as.difftime(x)})
  )


})
#> Test passed
