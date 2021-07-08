context("agg_table")

# hourly recorded data
path_file <- system.file('extdata', 'ianigla_cuevas.csv',
                         package = 'hydrotoolbox')


cuevas <- read_ianigla(path = path_file,
                       out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
                                    'p(mm)', 'wspd(km/hr)', 'wdir(°)',
                                    'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' ) )

# daily recorded data
path_file <- system.file('extdata', 'snih_qd_guido.xlsx',
                         package = 'hydrotoolbox')

guido <- read_snih(path = path_file, by = 'day', out_name = 'Q(m3/s)')


# first try to break the code arguments

test_that("x bad arguments", {
  expect_error( agg_table(x = 1:10) )
  expect_error( agg_table(x = mtcars) )
  expect_error( agg_table(x = data.frame(Date = cuevas$date, 'hola') ) )
  expect_error( agg_table(x = cuevas) )
})
#> Test passed

test_that("col_name bad arguments", {
  expect_error( agg_table(x = cuevas, col_name = 1) )
  expect_error( agg_table(x = cuevas, col_name = "hola", fun = "mean", period = "climatic") )
  expect_error( agg_table(x = cuevas, col_name = c("date", "rh(%)"), fun = "mean",
                          period = "climatic") )
})
#> Test passed

test_that("fun bad arguments", {
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = FALSE) )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = 'somebody_there',
                          period = 'monthly') )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean', 'hello'),
                          period = 'daily') )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean', 'min'),
                          period = 'daily') )
})
#> Test passed

test_that("period bad arguments", {
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'), period = 1:10) )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'), period = 'rock') )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'),
                          period = c('monthly', 'ratt') ) )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'),
                          period = c('monthly', 'daily') ) )
})
#> Test passed

test_that("out_name bad argument", {
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'),
                          period = 'monthly', out_name = 15) )
  expect_error( agg_table(x = guido, col_name = c('Q(m3/s)'), fun = c('mean'),
                          period = 'monthly', out_name = c('hi', 'two') ) )
})
#> Test passed

