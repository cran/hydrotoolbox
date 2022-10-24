## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval=FALSE, fig.width = 6, fig.height = 4----------------------
#  # set path to file
#  path_tmax <- system.file('extdata', 'cr2_tmax_yeso_embalse.csv',
#               package = 'hydrotoolbox')
#  
#  # read file with default colname
#  head( read_cr2(path = path_tmax) )
#  
#  # assign a column name
#  head( read_cr2(path = path_tmax, out_name = 'tmax(°C)') )

## ----build, eval = FALSE, fig.width = 6, fig.height = 4-----------------------
#  # path to all example files
#  path <- system.file('extdata', package = 'hydrotoolbox')
#  
#  # cr2 file
#  yeso <-
#    hm_create() %>%
#    hm_build_generic(path = path,
#                     file_name = "cr2_tmax_yeso_embalse.csv",
#                     slot_name = c('tmax'),
#                     by = "day",
#                     out_name = list("tmax_degC"),
#                     FUN = read_cr2
#                     )

