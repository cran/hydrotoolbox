## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval=FALSE, fig.width = 6, fig.height = 4----------------------
#  # set path to file
#  path <- system.file('extdata', 'mnemos_guido.xlsx', package = 'hydrotoolbox')
#  
#  # as we can have multiple sheets, we can get an idea of its content
#  read_mnemos(path = path, get_sheet = TRUE)
#  
#  # suppose that we want to read the daily minimum temperature
#  tmax_guido <- read_mnemos(path = path, by = 'day',
#                            out_name = 'tmax(ºC)', sheet = '11413-017')
#  
#  plot(x = tmax_guido[ , 1], y = tmax_guido[ , 2],
#       col = 'dodgerblue', type = 'l',
#       xlab = 'date', ylab = 'Tmin(ºC)')
#  

## ----build, eval = FALSE, fig.width = 6, fig.height = 4-----------------------
#  # in this path you will find the raw example data
#  path <- system.file('extdata', package = 'hydrotoolbox')
#  
#  list.files(path)
#  
#  # we load in a single object (hydromet_station class)
#  # the streamflow and water height series
#  guido <-
#    hm_create() %>% # create the met-station
#    hm_build(bureau = 'mnemos', path = path,
#             file_name = 'mnemos_guido.xlsx',
#             slot_name = c('qd', 'evap', 'tair',
#                           'tmax', 'tmin', 'wspd'),
#             by = c('day', 'day', '6 hour',
#                    'day', 'day', '6 hour'),
#             out_name = c('qd(m3/s)', 'e(mm/d)', 'tdb(ºC)',
#                          'tmax(ºC)', 'tmin(ºC)', 'w(km/h)') )
#  
#  # we can explore the data-set inside it by using hm_show
#  guido %>% hm_show()

