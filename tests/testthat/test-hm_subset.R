#context("hm_subset")

# lets work with the cuevas station
path <- system.file('extdata', package = 'hydrotoolbox')

# use the build method
cuevas <-
  hm_create() %>%
  hm_build(bureau = 'ianigla', path = path,
           file_name = 'ianigla_cuevas.csv',
           slot_name = c('tair', 'rh', 'patm',
                         'precip', 'wspd', 'wdir',
                         'kin', 'hsnow', 'tsoil'),
           by = 'hour',
           out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
                        'p(mm)', 'wspd(km/hr)', 'wdir(°)',
                        'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
  )

# try to break the code arguments

test_that("obj bad entry", {
  expect_error(
    hm_subset(obj = cuvas)
  )

  expect_error(
    hm_subset(obj = 'cuevas')
  )

  expect_error(
    hm_subset(obj = TRUE)
  )

  expect_error(
    hm_subset(obj = 1:10)
  )


})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_subset(obj = cuevas,
              slot_name = c('all', 'any'))
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = c( 'hi'))
  )

  expect_error(
    hm_subset(obj = cuevas,
              slot_name = c(1))
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = c(TRUE))
  )



})
#> Test passed


test_that("from bad entry", {
  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              from = as.Date('2019-01-01')
              )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              from = TRUE
    )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              from = 1
    )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              from = 'any'
    )
  )




})
#> Test passed


test_that("to bad entry", {
  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              to = as.Date('2019-01-01')
    )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              to = TRUE
    )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              to = 1
    )
  )


  expect_error(
    hm_subset(obj = cuevas,
              slot_name = 'rh',
              to = 'any'
    )
  )




})
#> Test passed
