## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, eval = FALSE--------------------------------------------------
#  library(tidyhydat)
#  library(dplyr)
#  library(hydrotoolbox)

## ----download, eval = FALSE---------------------------------------------------
#  download_hydat()

## ----station, eval = FALSE----------------------------------------------------
#  # Smith Creek near Marchwell, in Saskatchewan
#  station_number <- "05ME007"
#  
#  daily_vals <-
#    hy_daily_flows(station_number)
#  
#  
#  station_data <-
#    hm_create() %>%
#    hm_set(qd = daily_vals[ , c("Date", "Value")])

## ----metadata, eval = FALSE---------------------------------------------------
#  meta_data <- hy_stations(station_number)
#  
#  station_data <- hm_set(station_data,
#                         id = meta_data$STATION_NUMBER[1],
#                         station = meta_data$STATION_NAME[1],
#                         country = "Canada",
#                         lat = meta_data$LATITUDE[1],
#                         long = meta_data$LONGITUDE[1],
#                         province = meta_data$PROV_TERR_STATE_LOC[1],
#                         active = (meta_data$HYD_STATUS[1] == "ACTIVE"),
#                         basin_area = meta_data$DRAINAGE_AREA_GROSS[1],
#                         basin_eff = meta_data$DRAINAGE_AREA_EFFECT[1],
#                         other_1 = paste("RHBN:", meta_data$RHBN[1]))

## ----daily_plot, fig.width = 6, fig.height = 4, eval = FALSE------------------
#  hm_plot(obj = station_data,
#          slot_name = "qd",
#          col_name = list("Value"))

## ----monthly_plot, fig.width = 6, fig.height = 4, eval = FALSE----------------
#  monthly <- hm_agg(station_data,
#                    slot_name = "qd",
#                    fun = "mean",
#                    period = "monthly",
#                    col_name = "Value")
#  
#  monthly %>%
#    hm_plot(slot_name = "qd",
#            col_name = list("Value_mean")
#            )

## ----annual_plot, fig.width = 6, fig.height = 4, warning = FALSE, eval = FALSE----
#  
#  yearly <- hm_agg(station_data,
#                   slot_name = "qd",
#                   fun = "mean",
#                   period = "annually",
#                   col_name = "Value")
#  
#  yearly %>%
#    hm_plot(slot_name = "qd",
#            col_name = list("Value_mean"))

## ----my_fun, eval = FALSE-----------------------------------------------------
#  # before running this function, the packages
#  # tidyhydat and hydrotoolbox should be attached
#  
#  # station_number: character with station ID
#  build_hydat <- function(station_number){
#    # get daily flow series
#    q <-
#      hy_daily_flows(station_number) %>%
#      as.data.frame()
#  
#    # get station metadata
#    meta_table <-
#      hy_stations(station_number) %>%
#      as.data.frame()
#  
#    # set values
#    out_hm <-
#      hm_create() %>%
#      hm_set(qd = q[ , c("Date", "Value")],
#             id = meta_table$STATION_NUMBER[1],
#             station = meta_table$STATION_NAME[1],
#             country = "Canada",
#             lat = meta_table$LATITUDE[1],
#             long = meta_table$LONGITUDE[1],
#             province = meta_table$PROV_TERR_STATE_LOC[1],
#             active = (meta_table$HYD_STATUS[1] == "ACTIVE"),
#             basin_area = meta_table$DRAINAGE_AREA_GROSS[1],
#             basin_eff = meta_table$DRAINAGE_AREA_EFFECT[1],
#             other_1 = paste("RHBN:", meta_table$RHBN[1]))
#  }

## ----fun_applied, eval = FALSE------------------------------------------------
#  # we construct the Smith Creek near Marchwell
#  # but in a single code line
#  
#  smith_creek <- build_hydat(station_number = "05ME007")
#  
#  smith_creek %>%
#    hm_show(slot_name = c("id", "station", "country", "qd"))

