#context("hm_report")

# cuevas station
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
    hm_report(obj = 'cuevas', slot_name = 'rh')
  )

  expect_error(
    hm_report(obj = cuvas, slot_name = 'rh')
  )

  expect_error(
    hm_report(obj = TRUE, slot_name = 'rh')
  )


  expect_error(
    hm_report(obj = 1, slot_name = 'rh')
  )


})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_report(obj = cuevas,
              slot_name = 'any')
  )

  expect_error(
  hm_report(obj = cuevas,
            slot_name = c('tair', 'rh'))
  )

  expect_error(
    hm_report(obj = cuevas,
              slot_name = TRUE)
  )


  expect_error(
    hm_report(obj = cuevas,
              slot_name = 1:10)
  )



})
#> Test passed


test_that("col_name bad entry", {
  expect_error(
    hm_report(obj = cuevas,
              slot_name = 'rh',
              col_name = 'rh')
  )

  expect_error(
    hm_report(obj = cuevas,
              slot_name = 'rh',
              col_name = `rh(%)`)
  )

  expect_error(
    hm_report(obj = cuevas,
              slot_name = 'rh',
              col_name = 1)
  )

  expect_error(
    hm_report(obj = cuevas,
              slot_name = 'rh',
              col_name = TRUE)
  )


})
#> Test passed
