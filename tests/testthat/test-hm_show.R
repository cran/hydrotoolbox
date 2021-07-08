context("hm_show")

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
    hm_show(obj = cuvas)
  )

  expect_error(
    hm_show(obj = 'cuevas')
  )

  expect_error(
    hm_show(obj = TRUE)
  )

  expect_error(
    hm_show(obj = 1:10)
  )


})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_show(obj = cuevas,
            slot_name = c('tair', 'fill') )
  )

  expect_error(
    hm_show(obj = cuevas,
            slot_name = c(month.abb) )
  )

  expect_error(
  hm_show(obj = cuevas,
          slot_name = TRUE )
  )

  expect_error(
    hm_show(obj = cuevas,
            slot_name = 1:10 )
  )



})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_show(obj = cuevas,
            slot_name = 'rh',
            show = c('head', 'tail')  )
  )


  expect_error(
    hm_show(obj = cuevas,
            slot_name = 'rh',
            show = 'any'  )
  )


  expect_error(
    hm_show(obj = cuevas,
            slot_name = 'rh',
            show = 'any'  )
  )


  expect_error(
    hm_show(obj = cuevas,
            slot_name = 'rh',
            show = TRUE  )
  )

  expect_error(
    hm_show(obj = cuevas,
            slot_name = 'rh',
            show = 1:10  )
  )





})
#> Test passed
