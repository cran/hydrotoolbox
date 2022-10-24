#context("hm_plot")

# lets work with the cuevas station
path <- system.file('extdata', package = 'hydrotoolbox')

# use the build method
cuevas <-
  hm_create() %>%
  hm_build(bureau = 'ianigla', path = path,
           file_name = 'ianigla_cuevas.csv',
           slot_name = c('tair', 'rh', 'patm',
                         'precip', 'wspd', 'wdir',
                         'kin', 'hsnow', 'tsoil'),
           by = 'hour',
           out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
                        'p(mm)', 'wspd(km/hr)', 'wdir(°)',
                        'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
  )


# try to break the code arguments


test_that("obj bad entry", {
  expect_error(
    hm_plot(obj = TRUE,
            slot_name = 'tair',
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = 1:10,
            slot_name = 'tair',
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = 'cuevas',
            slot_name = 'tair',
            col_name = list('tair(°C)') )
  )

})
#> Test passed


test_that("slot_name bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'hello', 'any'),
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = mtcars,
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = TRUE,
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 1:10,
            col_name = list('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = slotNames(x = 'hydromet_station'),
            col_name = list('tair(°C)') )
  )



})
#> Test passed


test_that("col_name bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = c('tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list() )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list(TRUE) )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list(1:10) )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list(1) )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list('date', 'tair(°C)') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list(c('tair(°C)', 'any') ) )
  )

})
#> Test passed


test_that("interactive bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list('tair(°C)'),
            interactive = 1:10 )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list('tair(°C)'),
            interactive = 1 )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list('tair(°C)'),
            interactive = 'TRUE' )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = 'tair',
            col_name = list('tair(°C)'),
            interactive = as.factor(TRUE) )
  )

})
#> Test passed


test_that("line_type bad entry (ggplot2)", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = FALSE,
            line_type =  c('twodash', 'solid', 'solid'))
  )

  expect_error(
    hm_plot(obj = cuevas,
          slot_name = c('tair', 'rh'),
          col_name = list('tair(°C)', 'rh(%)'),
          interactive = FALSE,
          line_type =  c('twodash', 'TRUE'))
)

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = FALSE,
            line_type =  c('twodash', 'lines'))
  )



})
#> Test passed


test_that("line_type bad entry (plotly)", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines', 'lines'))
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'TRUE'))
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = FALSE,
            line_type =  c('twodash', 'lines'))
  )



})
#> Test passed


test_that("line_color bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines'),
            line_color = TRUE)
  )

   expect_error(
      hm_plot(obj = cuevas,
              slot_name = c('tair', 'rh'),
              col_name = list('tair(°C)', 'rh(%)'),
              interactive = TRUE,
              line_type =  c('lines', 'lines'),
              line_color = c('any', 'blank'))
    )

    expect_error(
        hm_plot(obj = cuevas,
                slot_name = c('tair', 'rh'),
                col_name = list('tair(°C)', 'rh(%)'),
                interactive = TRUE,
                line_type =  c('lines', 'lines'),
                line_color = c('blue', 'red', 'yellow'))
      )

    expect_error(
      hm_plot(obj = cuevas,
              slot_name = c('tair', 'rh'),
              col_name = list('tair(°C)', 'rh(%)'),
              interactive = TRUE,
              line_type =  c('lines', 'lines'),
              line_color = c('blue', 'red', 'any'))
    )





})
#> Test passed


test_that("line_size bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines'),
            line_color = c('blue', 'red'),
            line_size = TRUE )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines'),
            line_color = c('blue', 'red'),
            line_size = 1:10 )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines'),
            line_color = c('blue', 'red'),
            line_size = c('1', '1') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = TRUE,
            line_type =  c('lines', 'lines'),
            line_color = c('blue', 'red'),
            line_size = c(TRUE, FALSE) )
  )



})
#> Test passed


test_that("line_alpha bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1, 5)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 0.1, 1)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(FALSE, TRUE)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(1)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c('1', '5')  )
  )



})
#> Test passed


test_that("x_lab bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date', 'hello')  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c(FALSE)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = 1:10  )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = 1  )
  )





})
#> Test passed


test_that("y_lab bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c(month.abb)  )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c(TRUE)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c(1:10)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c(month.abb),
            dual_yaxis =  c('left', 'right') )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = TRUE,
            dual_yaxis =  c('left', 'right') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c(TRUE, FALSE),
            dual_yaxis =  c('left', 'right') )
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = 1:10,
            dual_yaxis =  c('left', 'right') )
  )



})
#> Test passed


test_that("title_lab bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = c('my awesome plot', 'any')   )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = c(TRUE)   )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = c(1)   )
  )





})
#> Test passed


test_that("legend_lab bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = 'Cuevas station',
            legend_lab =  c(month.abb)  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = 'Cuevas station',
            legend_lab =  1:10  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = 'Cuevas station',
            legend_lab =  TRUE  )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            dual_yaxis = c('left', 'right'),
            title_lab = 'Cuevas station',
            legend_lab =  c(TRUE, FALSE)  )
  )




})
#> Test passed


test_that("dual_yaxis bad entry", {
  expect_error(
    suppressWarnings(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'left') )
    )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c(month.abb) )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('any', 'hi') )
  )







})
#> Test passed


test_that("from bad entry", {
  expect_error(
        hm_plot(obj = cuevas,
              slot_name = c('tair', 'rh'),
              col_name = list('tair(°C)', 'rh(%)'),
              interactive = F,
              line_type =  c('solid', 'solid'),
              line_color = c('blue', 'red'),
              line_size = c(2, 1),
              line_alpha = c(0.5, 1),
              x_lab = c('date'),
              y_lab = c('T(ºC)', 'RH(%)'),
              title_lab = 'Cuevas station',
              legend_lab =  c('Air temp.', 'Relat. humidity'),
              dual_yaxis = c('left', 'right'),
              from = 'char')
    )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            from = TRUE)
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            from = c(ISOdate(2019, 1, 1), ISOdate(2019, 1, 2)) )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            from = 1:10 )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            from = 1 )
  )







})
#> Test passed


test_that("to bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            to = 'char')
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            to = TRUE)
  )

  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            to = c(ISOdate(2019, 1, 1), ISOdate(2019, 1, 2)) )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            to = 1:10 )
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  c('solid', 'solid'),
            line_color = c('blue', 'red'),
            line_size = c(2, 1),
            line_alpha = c(0.5, 1),
            x_lab = c('date'),
            y_lab = c('T(ºC)', 'RH(%)'),
            title_lab = 'Cuevas station',
            legend_lab =  c('Air temp.', 'Relat. humidity'),
            dual_yaxis = c('left', 'right'),
            to = 1 )
  )







})
#> Test passed


test_that("scatter bad entry", {
  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  19,
            line_color = 'blue',
            line_size = 1,
            line_alpha = 0.5,
            x_lab = c('T'),
            y_lab = c('RH'),
            title_lab = 'Cuevas station',
            scatter = c('x', 'y', 'x'))
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  19,
            line_color = 'blue',
            line_size = 1,
            line_alpha = 0.5,
            x_lab = c('T'),
            y_lab = c('RH'),
            title_lab = 'Cuevas station',
            scatter = c(TRUE))
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  19,
            line_color = 'blue',
            line_size = 1,
            line_alpha = 0.5,
            x_lab = c('T'),
            y_lab = c('RH'),
            title_lab = 'Cuevas station',
            scatter = c(1))
  )


  expect_error(
    hm_plot(obj = cuevas,
            slot_name = c('tair', 'rh'),
            col_name = list('tair(°C)', 'rh(%)'),
            interactive = F,
            line_type =  19,
            line_color = 'blue',
            line_size = 1,
            line_alpha = 0.5,
            x_lab = c('T'),
            y_lab = c('RH'),
            title_lab = 'Cuevas station',
            scatter = c(month.abb))
  )








})
#> Test passed
