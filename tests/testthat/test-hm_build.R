context("hm_build")

# path to all example files
path <- system.file('extdata', package = 'hydrotoolbox')

# station
my_hm <- hm_create()


# try to break the code arguments
test_that("obj bad entry", {
  expect_error( hm_build(obj = hm_create(class_name = 'compact') ) )
})
#> Test passed

test_that("bureau bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = TRUE) )
  expect_error( hm_build(obj = my_hm, bureau = 1:10) )
  expect_error( hm_build(obj = my_hm, bureau = NA_character_) )
  expect_error( hm_build(obj = my_hm, bureau = c('aic', 'ianigla')) )
  expect_error( hm_build(obj = my_hm, bureau = c('hi')) )
})
#> Test passed

test_that("path bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = TRUE,
                         file_name = 'ianigla_cuevas.csv') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = 1:10,
                         file_name = 'ianigla_cuevas.csv') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = NA_character_,
                         file_name = 'ianigla_cuevas.csv') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path =  c('aic', 'ianigla'),
                         file_name = 'ianigla_cuevas.csv') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path =  'home',
                         file_name = 'ianigla_cuevas.csv') )
})
#> Test passed

test_that("file_name bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = TRUE) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 1:10) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = NA_character_,
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name =  c('aic', 'ianigla'),
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour') )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name =  'home',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour') )
})
#> Test passed

test_that("slot_name bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = TRUE,
                         by = 'hour' ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = 1:10,
                         by = 'hour' ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = NA_character_,
                         by = 'hour' ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = 'id',
                         by = 'hour' ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'precip', 'hola'),
                         by = 'hour' ) )

})
#> Test passed

test_that("by bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = TRUE ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 1:10 ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hola' ) )
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = c('day', 'hour') ) )

})
#> Test passed

test_that("out_name bad entry", {
  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour', out_name = TRUE ) )

  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour', out_name = 1:10 ) )

  expect_error( hm_build(obj = my_hm, bureau = 'ianigla', path = path,
                         file_name = 'ianigla_cuevas.csv',
                         slot_name = c('tair', 'rh', 'patm',
                                       'precip', 'wspd', 'wdir',
                                       'kin', 'hsnow', 'tsoil'),
                         by = 'hour', out_name = month.abb ) )

})
#> Test passed

