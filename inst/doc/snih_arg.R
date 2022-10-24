## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hydrotoolbox)

## ----read_fun, eval=FALSE, fig.width = 6, fig.height = 4----------------------
#  # set path to file
#  path_file <- system.file('extdata', 'snih_qd_guido.xlsx', package = 'hydrotoolbox')
#  
#  # read daily mean streamflow with default column name
#  guido_qd <- read_snih(path = path_file, by = 'day')
#  
#  head(guido_qd)
#  
#  # now we use the function with column name
#  rm(guido_qd)
#  guido_qd <- read_snih(path = path_file,  by = 'day',
#                        out_name = 'qd(m3/s)')
#  
#  head(guido_qd)
#  
#  # plot the series
#  plot(x = guido_qd[ , 1], y = guido_qd[ , 2], type = 'l',
#       main = 'Daily mean streamflow at Guido (Mendoza basin)',
#       xlab = 'Date', ylab = 'Q(m3/s)', col = 'dodgerblue', lwd = 1,
#       ylim = c(0, 200))

## ----build, eval=FALSE, fig.width = 6, fig.height = 4-------------------------
#  # in this path you will find the raw example data
#  path <- system.file('extdata', package = 'hydrotoolbox')
#  
#  list.files(path)
#  
#  # we load in a single object (hydromet_station class)
#  # the streamflow and water height series
#  guido <-
#    hm_create() %>% # create the met-station
#    hm_build_generic(path = path,
#                     file_name = c('snih_qd_guido.xlsx'),
#                     slot_name = c('qd'),
#                     FUN = read_excel,
#                     by = c('day'),
#                     sheet = 1L
#                     )
#  
#  # we can explore the data-set inside it by using hm_show
#  guido %>% hm_show()
#  
#  # you can also rename the column names
#  guido <-
#    guido %>%
#    hm_name(slot_name = 'qd',
#          col_name = 'q(m3/s)')
#  
#  guido %>% hm_show(slot_name = 'qd')

## ----plot_1, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE-------
#  # we ask hydrotolkit to show all the variables
#  # with data in our station
#  guido %>% hm_show()
#  
#  # if want to analyze the daily mean streamflow records
#  guido %>%
#    hm_plot(slot_name = 'qd',
#            col_name = list('q(m3/s)'),
#            interactive = TRUE,
#            line_color = 'dodgerblue',
#            x_lab = 'Date', y_lab = 'Q(m3/s)' )

## ----plot_2, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE-------
#  # just show the discharge for the hydrological year 2016/2017
#  # for publishing
#  guido %>%
#    hm_plot(slot_name = 'qd',
#            col_name = list('q(m3/s)'),
#            interactive = FALSE,
#            line_color = 'dodgerblue',
#            x_lab = 'Date', y_lab = 'Q(m3/s)',
#            from = '2016-07-01', to = '2017-06-30',
#            legend_lab = 'Guido station',
#            title_lab = 'Daily mean discharge' )

## ----show, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE---------
#  # the show method allows to get an idea about the stored variables
#  guido %>%
#    hm_show()
#  
#  # or maybe we want to specify the slots
#  guido %>%
#    hm_show(slot_name = c('id', 'qd', 'tair') )

## ----report, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE-------
#  # suppose that to get an idea about the basic statistics of our data
#  # and we want to know how many missing data we have
#  guido %>%
#    hm_report(slot_name = 'qd')

## ----get, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE----------
#  # now you want to extract the table
#  guido %>%
#    hm_get(slot_name = 'qd') %>%
#    head()

## ----mutate, eval=FALSE, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE----
#  # apply a moving average windows to streamflow records
#  guido %>%
#    hm_mutate(slot_name = 'qd',
#              FUN = mov_avg, k = 10,
#              pos = 'c', out_name = 'mov_avg') %>% # see ?mov_avg()
#    hm_plot(slot_name = 'qd',
#           col_name = list(c('q(m3/s)', 'mov_avg') ),
#           interactive = TRUE,
#           line_color = c('dodgerblue', 'red3'),
#           y_lab = 'Q(m3/s)',
#           legend_lab = c('obs', 'mov_avg')  )

## ----melt, eval=FALSE, fig.width = 6, fig.height = 4, warning = FALSE---------
#  # lets say that we want to put together snow water equivalent from Toscas (dgi)
#  # and daily streamflow discharge from Guido (snih)
#  
#  # on the first place we build the Toscas station
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
#  
#  # now we melt the required data in a new object
#  hm_create(class_name = 'compact') %>%
#       hm_melt(melt = c('toscas', 'guido'),
#               slot_name = list(toscas = 'swe', guido = 'qd'),
#               col_name = 'all',
#               out_name = c('swe(mm)', 'qd(m3/s)')
#               ) %>%
#         hm_plot(slot_name = 'compact',
#                 col_name = list( c('swe(mm)', 'qd(m3/s)') ),
#                 interactive = TRUE,
#                 legend_lab = c('swe-Toscas', 'qd-Guido'),
#                 line_color = c('dodgerblue', 'red'),
#                 y_lab = c('q(m3/s)', 'swe(mm)'),
#                 dual_yaxis = c('right', 'left')
#                  )

