#context("hm_agg")

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
test_that("slot_name bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = TRUE) )
  expect_error( hm_agg(obj = cuevas, slot_name = 1) )
  expect_error( hm_agg(obj = cuevas, slot_name = c('hola', 'chau')) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'qs') )
})
#> Test passed

test_that("col_name bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = TRUE ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'hola' ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'tair' ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 1:10 ) )
})
#> Test passed

test_that("fun bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'rh(%)' ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'rh(%)',
                       fun = TRUE ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'rh(%)',
                       fun = 1:10 ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'tair', col_name = 'rh(%)',
                       fun = c('mean', 'first') ) )
})
#> Test passed

test_that("period bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean') )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = TRUE) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 1:10) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = c('hourly', 'mean') ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = c('hourly', 'daily') ) )
})
#> Test passed


test_that("out_name bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', out_name = TRUE ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', out_name = 1:10 ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', out_name = month.abb ) )
})
#> Test passed

test_that("allow_na bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', allow_na = TRUE ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', allow_na = 1:10 ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', allow_na = '10' ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', allow_na = NA_character_ ) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'daily', allow_na = NA_real_ ) )
})
#> Test passed

test_that("start_month bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', start_month = NA_real_) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', start_month = '1') )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', start_month = 1:10) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', start_month = 15) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', start_month = TRUE) )
})
#> Test passed

test_that("end_month bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', end_month = NA_real_) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', end_month = '1') )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', end_month = 1:10) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', end_month = 15) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'annually', end_month = TRUE) )
})
#> Test passed


test_that("relocate bad entry", {
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'monthly', relocate = TRUE) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'monthly', relocate = 1:10) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'monthly', relocate = '1') )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'monthly', relocate = c('patm', 'hola')) )
  expect_error( hm_agg(obj = cuevas, slot_name = 'rh', col_name = 'rh(%)',
                       fun = 'mean', period = 'monthly', relocate = c('patm', 'tmax')) )
})
#> Test passed
