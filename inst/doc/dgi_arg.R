## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval=FALSE, fig.width = 6, fig.height = 4----------------------
#  # set path to file
#  path_file <- system.file('extdata', 'dgi_toscas.xlsx',
#               package = 'hydrotoolbox')
#  
#  # because dgi files has multiple sheets we take a look
#  # on them
#  read_dgi(path = path_file, get_sheet = TRUE)
#  
#  # read swe with default column names
#  head( read_dgi(path = path_file, sheet = 'swe') )
#  
#  # assign name
#  head( read_dgi(path = path_file, sheet = 'swe', out_name = 'swe(mm)') )

## ----build, eval = FALSE, fig.width = 6, fig.height = 4-----------------------
#  library(readxl)
#  # path to all example files
#  path <- system.file('extdata', package = 'hydrotoolbox')
#  
#  # dgi file
#  toscas <-
#    hm_create() %>%
#    hm_build_generic(path = path,
#                     file_name = 'dgi_toscas.xlsx',
#                     slot_name = c('swe', 'tmax',
#                                   'tmin', 'tmean',
#                                   'rh', 'patm'),
#                     by = 'day',
#                     FUN = read_dgi,
#                     sheet = 1L:6L )

