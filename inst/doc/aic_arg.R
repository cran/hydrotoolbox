## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval = FALSE, fig.width = 6, fig.height = 4--------------------
#  # if you want to get the sheet's names
#  read_aic(path = path, get_sheet = TRUE)
#  
#  # loading the series in a data.frame
#  my_data <-
#    read_aic(path = path,
#           by = 'day',
#           sheet = 'CERRO NEVADO')

## ----build, eval = FALSE, fig.width = 6, fig.height = 4-----------------------
#  # set path and file name
#  my_path <- '/home/.../my_folder'
#  my_file <- 'AIC_data.xls'
#  
#  # build the station
#  nevado <-
#    hm_create() %>%
#    hm_build(bureau = 'aic',
#             path = my_path,
#             file_name = my_file,
#             slot_name = c('precip', 'tmax', 'tmin',
#                           'tmean', 'rh', 'wspd',
#                           'wdir', 'swe'),
#             by = 'day',
#             sheet = 'CERRO NEVADO')

