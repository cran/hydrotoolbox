## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval=FALSE,fig.width = 6, fig.height = 4-----------------------
#  # set path to file
#  path_file <- system.file('extdata', 'ianigla_cuevas.csv',
#               package = 'hydrotoolbox')
#  
#  # read with default names
#  head( read_ianigla(path = path_file) )
#  
#  # set column names
#  head(
#  read_ianigla(path = path_file,
#               out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#                             'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#                             'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' ) )
#  )

## ----build, eval = FALSE, fig.width = 6, fig.height = 4-----------------------
#  # path to all example files
#  path <- system.file('extdata', package = 'hydrotoolbox')
#  
#  # ianigla file
#  cuevas <-
#    hm_create() %>%
#    hm_build(bureau = 'ianigla', path = path,
#             file_name = 'ianigla_cuevas.csv',
#             slot_name = c('tair', 'rh', 'patm',
#                           'precip', 'wspd', 'wdir',
#                           'kin', 'hsnow', 'tsoil'),
#             by = 'hour',
#             out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#                          'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#                          'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
#            )

