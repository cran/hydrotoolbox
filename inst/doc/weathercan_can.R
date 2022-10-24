## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, eval = FALSE--------------------------------------------------
#  library(weathercan)
#  library(hydrotoolbox)

## ----station, eval = FALSE----------------------------------------------------
#  # get station ID's
#  head(stations)
#  
#  # search by name
#  stations_search("Kamloops", interval = "day")
#  
#  # Kamloops A - BC province
#  station_id <- 1274
#  
#  kam <-
#    weather_dl(station_ids = station_id,
#               start = "1900-01-01",
#               end = "1950-12-31",
#               interval = "day") %>%
#    as.data.frame()
#  
#  # now we create the station object and we set the data
#  kamloops_hm <-
#    hm_create() %>%
#    hm_set(id = station_id,
#           station = kam$station_name[1],
#           province = kam$prov[1],
#           country = "Canada",
#           lat = kam$lat[1],
#           long = kam$lon[1],
#           alt = kam$elev[1],
#           tmean = kam[ , c("date", "mean_temp")],
#           tmax = kam[ , c("date", "max_temp")],
#           tmin = kam[ , c("date", "min_temp")],
#           precip = kam[ , c("date", "total_precip")],
#           rainfall = kam[ , c("date", "total_rain")]
#           )
#  
#  kamloops_hm %>% hm_show()
#  
#  # we plot air temperatures
#  kamloops_hm %>%
#    hm_plot(slot_name = c('tmean', 'tmax', 'tmin'),
#            col_name = list('mean_temp', 'max_temp', 'min_temp'),
#            interactive = TRUE,
#            line_color = c('forestgreen', 'red', 'dodgerblue'),
#            x_lab = 'Date', y_lab = 'T(ºC)',
#            legend_lab = c('mean', 'max', 'min') )

## ----my_fun, eval = FALSE-----------------------------------------------------
#  # before running this function, the packages
#  # weathercan and hydrotoolbox should be attached
#  
#  # station_number: character with station ID
#  build_weathercan <- function(station_id,
#                               from, to,
#                               time_step){
#  
#    # download station data
#    station <-
#      weather_dl(station_ids = station_id,
#               start = from,
#               end = to,
#               interval = time_step) %>%
#      as.data.frame()
#  
#    # now we create the station object and we set the (meta)data
#    station_hm <-
#      hm_create() %>%
#      hm_set(id = station_id,
#           station = station$station_name[1],
#           province = station$prov[1],
#           country = "Canada",
#           lat = station$lat[1],
#           long = station$lon[1],
#           alt = station$elev[1],
#           tmean = station[ , c("date", "mean_temp")],
#           tmax = station[ , c("date", "max_temp")],
#           tmin = station[ , c("date", "min_temp")],
#           precip = station[ , c("date", "total_precip")],
#           rainfall = station[ , c("date", "total_rain")]
#           )
#  
#    return(station_hm)
#  }

## ----fun_applied, eval = FALSE------------------------------------------------
#  # we construct the Kamloops station
#  # but in a single code line
#  
#  kamloops_station <-
#    build_weathercan(station_id = 1274,
#                     from = "1900-01-01",
#                     to = "1950-12-31",
#                     time_step = "day")
#  
#  kamloops_station %>%
#    hm_show()

