context("hm_melt")

# path to all example files
path <- system.file('extdata', package = 'hydrotoolbox')

# on the first place we build the stations
# dgi file
toscas <-
  hm_create() %>%
  hm_build(bureau = 'dgi', path = path,
           file_name = 'dgi_toscas.xlsx',
           slot_name = c('swe', 'tmax',
                         'tmin', 'tmean', 'rh', 'patm'),
           by = 'day',
           out_name = c('swe', 'tmax',
                        'tmin', 'tmean', 'rh', 'patm') )

# snih file
guido <-
  hm_create() %>%
  hm_build(bureau = 'snih', path = path,
           file_name = c('snih_hq_guido.xlsx',
                         'snih_qd_guido.xlsx'),
           slot_name = c('hq', 'qd'),
           by = c('none', 'day') )

# create compact object
my_comp <- hm_create(class_name = 'compact')

# try to break the code arguments
test_that("obj bad entry", {
  expect_error( hm_melt(obj = TRUE) )
  expect_error( hm_melt(obj = 1:10) )
  expect_error( hm_melt(obj = 'hola') )
  expect_error( hm_melt(obj = list(my_comp, my_comp)) )
})
#> Test passed


test_that("melt bad entry", {
  expect_error( hm_melt(obj = my_comp, melt = TRUE ) )
  expect_error( hm_melt(obj = my_comp, melt = 1:10 ) )
  expect_error( hm_melt(obj = my_comp, melt = c(guido, toscas) ) )
  expect_error( hm_melt(obj = my_comp, melt = c('guido', toscas) ) )
  expect_error( hm_melt(obj = my_comp, melt = c('guido', 'toscas', 'hello'),
                        slot_name = list(toscas = 'swe', guido = 'qd', hello = 'swe'),
                        col_name = 'all',
                        out_name = c('swe(mm)', 'qd(m3/s)', 'swe') ) )
})
#> Test passed


test_that("slot_name bad entry", {
  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = TRUE,
                        col_name = 'all',
                        out_name = c('swe(mm)', 'qd(m3/s)') )
                )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = 1:10,
                        col_name = 'all',
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = c(toscas = 'swe', guido = 'qd'),
                        col_name = 'all',
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list('hola', 'chau'),
                        col_name = 'all',
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )
})
#> Test passed


test_that("col_name bad entry", {
  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = 'alls',
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = FALSE,
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = c('toscas_swe_swe', 'guido_qd_Valor'),
                        out_name = c('swe(mm)', 'qd(m3/s)') )
  )

})
#> Test passed


test_that("out_name bad entry", {
  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = 'all',
                        out_name = FALSE )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = 'all',
                        out_name = 1:10 )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = 'all',
                        out_name = c('hola') )
  )

  expect_error( hm_melt(obj = my_comp,
                        melt = c('toscas', 'guido'),
                        slot_name = list(toscas = 'swe', guido = 'qd'),
                        col_name = 'all',
                        out_name = c('swe', 'qd', 'any') )
  )

})
#> Test passed
