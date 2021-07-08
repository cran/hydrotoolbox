# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Methods to easily use \code{ggplot2} or \code{plotly} (interactive)
#'
#' @description This method allows you to make plots (using simple and expressive arguments)
#'  of the variables contained inside an \code{hydromet_XXX} class object.
#'  The plot outputs can be static (\code{ggplot2}) or dynamic (\code{plotly}).
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string vector with the name of the slot(s) to use in plotting.
#' @param col_name list containing the column name of the variables to plot. Every element inside
#'  the list belongs to the previous defined slot(s).
#' @param interactive logical. Default value, \code{FALSE}, will return a \code{ggplot2}
#' class object. Otherwise you will get a \code{plotly} one.
#' @param line_type string with the name of the line dash type (\code{ggplot2}) or mode in
#'  the \code{plotly} case. \code{ggplot2}: \code{'solid'} (default value), \code{'twodash'},
#'   \code{'longdash'}, \code{'dotted'}, \code{'dotdash'}, \code{'dashed'} or \code{'blank'}.
#'   \code{plotly}: \code{'lines'} (default value), \code{'lines+markers'} or \code{'markers'}.
#'   \bold{NOTE:} when using \code{scatter} plot this arguments goes through the \code{shape}
#'   argument (in \code{geom_point()}) as numeric.
#' @param line_color string with a valid \code{color} name. See \code{'colors()'} or
#'  \href{http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf}{Rcolor document}.
#' @param line_size numeric vector containing the size of every line to plot. If you use
#' the \code{NULL} value it will return the plots with default(s) for either
#'  \code{ggplot2} or \code{plotly}.
#' @param line_alpha numeric vector with line(s) transparency. From 0 (invisible) to 1.
#' @param x_lab string with \code{x axis} label. Default is \code{'Date'}.
#' @param y_lab string with \code{y axis} label. In case you use \code{dual_yaxis}
#'  argument you must supply both \code{c('ylab', 'y2lab')}.
#' @param title_lab string with the title of the plot. Default is a plot without title.
#' @param legend_lab string vector with plot label(s) name(s).
#' @param dual_yaxis string vector suggesting which variables are assign either to
#'  the \code{'left'} or \code{'right'} y axis.
#' @param from string value for \code{'Date'} class or \code{POSIXct} class
#' for date-time data with the starting \code{Date}. You can use \code{'from'}
#' without \code{'to'}. In this case you will subset your data \code{'from'} till the end.
#' @param to string value for \code{'Date'} class or \code{POSIXct} class for date-time
#'  data with the ending \code{Date}. You can use \code{'to'} without \code{'from'}.
#'  In this case you will subset your data from the beginning till \code{'to'}.
#' @param scatter string vector (of length two) suggesting which variables goes in
#'  the \code{'x'} and \code{'y'} axis respectively. Valid character entries
#'  are \code{'x'} and \code{'y'}.
#'
#' @return A \code{ggplot2} or \code{plotly} object.
#'
#' @import ggplot2
#' @importFrom plotly plot_ly add_trace layout %>% ggplotly
#' @importFrom grDevices colors heat.colors
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # lets work with the cuevas station
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # use the build method
#' hm_cuevas <-
#'   hm_create() %>%
#'   hm_build(bureau = 'ianigla', path = path,
#'            file_name = 'ianigla_cuevas.csv',
#'            slot_name = c('tair', 'rh', 'patm',
#'                          'precip', 'wspd', 'wdir',
#'                          'kin', 'hsnow', 'tsoil'),
#'            by = 'hour',
#'            out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                         'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                         'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
#'            )
#'
#' # let's start by making a single variable static plot
#' hm_plot(obj = hm_cuevas, slot_name = 'tair',
#'         col_name = list('tair(°C)') )
#'
#' # we add labels, change color, line type and we focus
#' # on specific date range
#' hm_plot(obj = hm_cuevas, slot_name = 'tair',
#'         col_name = list('tair(°C)'),
#'         line_type = 'longdash',
#'         line_color = 'dodgerblue',
#'         x_lab = 'Date time', y_lab = 'T(°C)',
#'         title_lab = 'Hourly temperature at Cuevas',
#'         legend_lab = 'Tair',
#'         from = ISOdate(2020, 7, 1),
#'         to = ISOdate(2020, 7, 5))
#'
#' # compare air with soil temperature
#' hm_plot(obj = hm_cuevas, slot_name = c('tair', 'tsoil'),
#'         col_name = list('tair(°C)', 'tsoil(°C)'),
#'         line_type = c('longdash', 'solid'),
#'         line_color = c('dodgerblue', 'tan4'),
#'         x_lab = 'Date time', y_lab = 'T(°C)',
#'         title_lab = 'Hourly temperature at Cuevas',
#'         legend_lab = c('Tair', 'Tsoil'),
#'         from = ISOdate(2020, 7, 1),
#'         to = ISOdate(2020, 7, 5))
#'
#' # let's add relative humidity on the right y-axis
#' hm_plot(obj = hm_cuevas, slot_name = c('tair', 'tsoil', 'rh'),
#'         col_name = list('tair(°C)', 'tsoil(°C)', 'rh(%)'),
#'         line_type = c('longdash', 'solid', 'solid'),
#'         line_color = c('dodgerblue', 'tan4', 'red'),
#'         x_lab = 'Date time', y_lab = c('T(°C)', 'RH(%)'),
#'         title_lab = 'Hourly meteo data at Cuevas',
#'         legend_lab = c('Tair', 'Tsoil', 'RH'),
#'         dual_yaxis = c('left', 'left', 'right'),
#'         from = ISOdate(2020, 7, 1),
#'         to = ISOdate(2020, 7, 5))
#'
#' # we decide to analize the previous variables in detail
#' # with a dynamic plot
#' hm_plot(obj = hm_cuevas, slot_name = c('tair', 'tsoil', 'rh'),
#'         col_name = list('tair(°C)', 'tsoil(°C)', 'rh(%)'),
#'         line_color = c('dodgerblue', 'tan4', 'red'),
#'         x_lab = 'Date time', y_lab = c('T(°C)', 'RH(%)'),
#'         title_lab = 'Hourly meteo data at Cuevas',
#'         legend_lab = c('Tair', 'Tsoil', 'RH'),
#'         dual_yaxis = c('left', 'left', 'right'),
#'         interactive = TRUE)
#' # click on the Zoom icon and play a little...
#'
#'
#' # suppose now that we want to make a scatter plot to show
#' # the negative correlation between air temperature and
#' # relative humidity
#' hm_plot(obj = hm_cuevas, slot_name = c('tair', 'rh'),
#'         col_name = list('tair(°C)', 'rh(%)'),
#'         line_color = 'dodgerblue',
#'         x_lab = 'Tair', y_lab = 'RH',
#'         scatter = c('x', 'y') )
#'}
#'
#'
setGeneric(name = 'hm_plot',
           def = function(obj, slot_name, col_name, interactive = FALSE,
                          line_type = NULL, line_color = NULL,
                          line_size = NULL, line_alpha = NULL,
                          x_lab = 'date', y_lab = 'y', title_lab = NULL,
                          legend_lab = NULL, dual_yaxis = NULL,
                          from = NULL, to = NULL, scatter = NULL)
           {
             standardGeneric('hm_plot')
           })


#' @describeIn hm_plot plot method for station class
## station
setMethod(f = 'hm_plot',
          signature = 'hydromet_station',
          definition = function(obj, slot_name, col_name, interactive = FALSE,
                                line_type = NULL, line_color = NULL,
                                line_size = NULL, line_alpha = NULL,
                                x_lab = 'date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, dual_yaxis = NULL,
                                from = NULL, to = NULL, scatter = NULL)
          {
            #**************************
            #* conditionals
            #**************************
            #*obj
            check_class(argument = obj, target = 'hydromet_station', arg_name = 'obj')

            #*slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name, target = slotNames(x = 'hydromet_station')[1:23],
                         arg_name = 'slot_name')

            #*col_name
            check_class(argument = col_name, target = c('list'), arg_name = 'col_name')

            col_name_vect <- unlist(col_name) # for internal operations
            check_class(argument = col_name_vect, target = 'character',
                        arg_name = 'col_name internal arguments')

              # for check string
            column_names <- list()
            for(i in 1:length(slot_name)){
              column_names[[i]] <- colnames( hm_get(obj = obj, slot_name = slot_name[i]) )[-1]
            }
            check_string(argument = col_name_vect, target = unlist(column_names),
                         arg_name = 'col_name')



            #*interactive
            check_class(argument = interactive, target = 'logical', arg_name = 'interactive')
            check_length(argument = interactive, max_allow = 1, arg_name = 'interactive')

            #*line_type
            if( is.null(scatter) ){
              # line plots
              check_class(argument = line_type, target = 'character', arg_name = 'line_type')
              if(interactive == FALSE){
                # ggplot2
                check_string(argument = line_type, target = c('solid', 'twodash',
                             'longdash', 'dotted', 'dotdash', 'dashed', 'blank'),
                             arg_name = 'line_type')

              } else{
                #plotly
                check_string(argument = line_type, target = c('lines', 'lines+markers',
                            'markers'),
                             arg_name = 'line_type')
              } # string check
              check_cross(ref_arg = col_name_vect, eval_arg = line_type,
                          arg_names = c('col_name', 'line_type') )

            } else{
              # scatter plots
              check_class(argument = line_type, target = 'numeric', arg_name = 'line_type (point shape)')
              check_numeric(argument = line_type, target = 0:24, arg_name = 'line_type (point shape)')
              check_length(argument = line_type, max_allow = 1, arg_name = 'line_type (point shape)')

            }

            #*line_color
            check_class(argument = line_color, target = 'character', arg_name = 'line_color')
            check_string(argument = line_color, target = colors(), arg_name = 'line_color')
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_color,
                          arg_names = c('col_name', 'line_color'))
            } else{
              # scatter
              check_length(argument = line_color, max_allow = 1, arg_name = 'line_color (scatter)')
            }

            #*line_size
            check_class(argument = line_size, target = c('numeric', 'integer'),
                        arg_name = 'line_size')

              # for valid size values
            check_values <- sum( which(line_size < 0) )
            if(check_values != 0){ stop('line_size argument(s) shuld be greater than 0!',
                                        call. = FALSE) }

              # for cross validation
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_size,
                          arg_names = c('col_name', 'line_size'))
            } else{
              # scatter
              check_length(argument = line_size, max_allow = 1, arg_name = 'line_size (scatter)')
            }


            #*line_alpha
            check_class(argument = line_alpha, target = 'numeric', arg_name = 'line_alpha')

            # for valid size values
            check_values <- sum( which(line_alpha < 0 | line_alpha > 1 ) )
            if(check_values != 0){ stop('line_alpha argument(s) shuld be between 0 and 1!',
                                        call. = FALSE) }

            # for cross validation
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_alpha,
                          arg_names = c('col_name', 'line_alpha'))
            } else{
              # scatter
              check_length(argument = line_alpha, max_allow = 1, arg_name = 'line_alpha (scatter)')
            }


            #* xlab
            check_class(argument = x_lab, target = 'character', arg_name = 'x_lab')
            check_length(argument = x_lab, max_allow = 1, arg_name = 'x_lab')

            #* ylab
            check_class(argument = y_lab, target = 'character', arg_name = 'y_lab')
            if( is.null(dual_yaxis) ){
              # single y axis
              check_length(argument = y_lab, max_allow = 1, arg_name = 'y_lab')

            } else{
              # dual y axis
              check_length(argument = y_lab, max_allow = 2, arg_name = 'y_lab')
            }

            #* title_lab
            check_class(argument = title_lab, target = 'character', arg_name = 'title_lab')
            check_length(argument = title_lab, max_allow = 1, arg_name = 'title_lab')


            #* legend_lab
            check_class(argument = legend_lab, target = 'character', arg_name = 'legend_lab')
            check_cross(ref_arg = col_name_vect, eval_arg = legend_lab,
                        arg_names = c('col_name', 'legend_lab'))

            #* dual_yaxis
            check_class(argument = dual_yaxis, target = 'character', arg_name = 'dual_yaxis')
            check_string(argument = dual_yaxis, target = c('left', 'right'), arg_name = 'dual_yaxis')
            check_cross(ref_arg = col_name_vect, eval_arg = dual_yaxis,
                        arg_names = c('col_name', 'dual_yaxis'))

            #* from
            check_class(argument = from, target = c('character', 'POSIXct'), arg_name = 'from')
            check_length(argument = from, max_allow = 1, arg_name = 'from')

            #* to
            check_class(argument = to, target = c('character', 'POSIXct'), arg_name = 'to')
            check_length(argument = to, max_allow = 1, arg_name = 'to')

            #* scatter
            check_class(argument = scatter, target = 'character', arg_name = 'scatter')
            check_string(argument = scatter, target = c('x', 'y'), arg_name = 'scatter')
            check_cross(ref_arg = col_name_vect, eval_arg = scatter, arg_names = 'scatter')


            #**************************
            #* set default arg values
            #**************************
            #* line_type
            if( is.null(line_type) ){

              if( is.null(scatter) ){
                # line plot

                if(interactive == FALSE){
                  # ggplot2
                  line_type <- rep('solid', length(col_name_vect))

                } else {
                  # plotly
                  line_type <- rep('lines', length(col_name_vect))

                }

              } else{
                  # scatter plot
                line_type <- 16

                }


            }

            #* line_color
            if( is.null(line_color) ){

              if( is.null(scatter) ){
                # line plot
                line_color <- heat.colors(n = length(col_name_vect) )

              } else{
                # scatter plot
                line_color <- 'dodgerblue'

              }


            }

            #* line_size
            if( is.null(line_size) ){

              if( is.null(scatter) ){
                # line plot
                line_size <- rep(0.8, length(col_name_vect) )

              } else{
                # scatter plot
                line_size <- 1

              }


            }

            #* line_alpha
            if( is.null(line_alpha) ){

              if( is.null(scatter) ){
                # line plot
                line_alpha <- rep(1, length(col_name_vect) )

              } else{
                # scatter plot
                line_alpha <- 1

              }


            }

            #* y_lab
            if( !is.null(dual_yaxis) ){
              if( length(y_lab) != 2){
                y_lab <- c('y', 'y2')
              }

            }


            #**************************
            #* function
            #**************************
            #* build classic table (subsetted)
            c_table <- build_table(hm_obj = obj, slot_name = slot_name,
                                   col_name = col_name, from = from, to = to)


            #* is it interactive?
            if(interactive == FALSE){
              #* ggplot2


              #* scatter plot?
              if( is.null(scatter) == TRUE ) {
                #* line plot

                #* do we need double y axis?
                if( is.null(dual_yaxis) == TRUE ){
                  #* single y axis

                  #* transform to ggplot table
                  gg_table <- ggplot_table(df = c_table, l_color = line_color,
                                           l_type = line_type, l_size = line_size,
                                           l_legend = legend_lab)

                  #* ggplot2 object
                  gg_out <-
                    ggplot(data = gg_table,
                           aes_string(x = 'date', y = 'series', group = 'group') ) +
                    geom_line( aes_string(size = 'group', linetype = 'group', color = 'group', alpha = 'group')  ) +
                    scale_color_manual( values = line_color ) +
                    scale_linetype_manual(values = line_type ) +
                    scale_size_manual(values = line_size ) +
                    scale_alpha_manual(values = line_alpha) +
                    theme(legend.title = element_blank()) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)


                  return(gg_out)



                } else{
                  #* dual y axis


                  #* rescale the table
                  index_left  <- which(dual_yaxis == 'left')
                  index_right <- which(dual_yaxis == 'right')

                  nm_table    <- colnames(c_table)[-1] # variable names
                  left_names  <- nm_table[index_left]
                  right_names <- nm_table[index_right]

                  dual_list <- dual_y_table(df = c_table,
                                             y_left = left_names,
                                             y_right = right_names)

                  dual_table <- dual_list[['table']]

                  #* transform to ggplot table
                  gg_table <- ggplot_table(df = dual_table,
                                          l_color = line_color,
                                          l_type = line_type,
                                          l_size = line_size,
                                          l_legend = legend_lab)

                  #* get coefficients inside the environment
                  a  <- as.numeric( dual_list[['coefficients']][1] )
                  b  <- as.numeric( dual_list[['coefficients']][2] )
                  sf <- as.numeric( dual_list[['coefficients']][3] )

                  trans <- dual_list[['transformation']]


                  #* ggplot2 object
                  gg_out <-
                    ggplot(data = gg_table,
                           aes_string(x = 'date', y = 'series', group = 'group') ) +
                    geom_line( aes_string(size = 'group', linetype = 'group', color = 'group', alpha = 'group')  ) +
                    scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]) ) +
                    scale_color_manual( values = line_color ) +
                    scale_linetype_manual(values = line_type ) +
                    scale_size_manual(values = line_size ) +
                    scale_alpha_manual(values = line_alpha) +
                    theme(legend.title = element_blank()) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)


                  return(gg_out)

                }
              } else {
                #* scatter plot


                #* get x and y variable position
                index_x <- which(scatter == 'x')
                index_y <- which(scatter == 'y')

                nm_table <- colnames(c_table)[-1] # variable names
                x_name   <- nm_table[index_x]
                y_name   <- nm_table[index_y]

                #* ggplot2 object
                gg_out <-
                  ggplot(data = c_table,
                         aes(x = c_table[ , x_name], y = c_table[ , y_name]) ) +
                  geom_point(size = line_size, color = line_color, alpha = line_alpha, shape = line_type ) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)


                return(gg_out)



              }





            } else{
              #* plotly

              #* scatter plot?
              if( is.null(scatter) == TRUE ){
                # line plot

                # dual y axis?
                if( is.null(dual_yaxis) == TRUE ){
                  # single y axis

                  # basic plotly
                  ppout   <- plot_ly(c_table, x = ~date)
                  n_plots <- ncol(c_table) - 1

                  # build graphs without axis labels
                  for(i in 1:n_plots){
                    ppout <- ppout %>%
                      add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                line = list( width = line_size[i]), opacity = line_alpha[i] )

                  }# end for

                  # add axis labels
                  ppout <-
                    ppout %>%
                    layout(title = title_lab,
                           xaxis = list(title = x_lab),
                           yaxis = list(title = y_lab) )

                  # return
                  return(ppout)

                } else {
                  # dual y axis

                  # basic plotly
                  ppout   <- plot_ly(c_table, x = ~date)
                  n_plots <- ncol(c_table) - 1

                  # genero graficos sin etiquetas en los ejes
                  for(i in 1:n_plots){

                    if(dual_yaxis[i] == 'left'){
                      # left axis
                      ppout <- ppout %>%
                        add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                  type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                  line = list( width = line_size[i]), opacity = line_alpha[i] )

                    } else if (dual_yaxis[i] == 'right'){
                      # right axis
                      ppout <- ppout %>%
                        add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                  type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                  line = list( width = line_size[i]), opacity = line_alpha[i],
                                  yaxis = 'y2')

                    }


                  }# end for

                  # add labels
                  ppout <-
                    ppout %>%
                    layout(title  = title_lab,
                           xaxis  = list(title = x_lab),
                           yaxis  = list(title = y_lab[1]),
                           yaxis2 = list(title = y_lab[2],
                                         overlaying = 'y',
                                         side = 'right')  )

                  # return
                  return(ppout)

                }

              } else {
                #* scatter plot


                #* get x and y variable position
                index_x <- which(scatter == 'x')
                index_y <- which(scatter == 'y')

                nm_table <- colnames(c_table)[-1] # variable names
                x_name   <- nm_table[index_x]
                y_name   <- nm_table[index_y]

                #* ggplot2 object
                gg_out <-
                  ggplot(data = c_table,
                         aes(x = c_table[ , x_name], y = c_table[ , y_name]) ) +
                  geom_point(size = line_size, color = line_color, alpha = line_alpha, shape = line_type ) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)

                # turn ggplot to plotly
                pp_out <- ggplotly( gg_out )

                # return
                return(pp_out)

              }




            }


          })



#' @describeIn hm_plot plot method for compact class
## compact
setMethod(f = 'hm_plot',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name, col_name, interactive = FALSE,
                                line_type = NULL, line_color = NULL,
                                line_size = NULL, line_alpha = NULL,
                                x_lab = 'date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, dual_yaxis = NULL,
                                from = NULL, to = NULL, scatter = NULL)
          {
            #**************************
            #* conditionals
            #**************************
            #*obj
            check_class(argument = obj, target = 'hydromet_compact', arg_name = 'obj')

            #*slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name, target = 'compact',
                         arg_name = 'slot_name')

            #*col_name
            check_class(argument = col_name, target = c('list'), arg_name = 'col_name')

            col_name_vect <- unlist(col_name) # for internal operations
            check_class(argument = col_name_vect, target = 'character',
                        arg_name = 'col_name internal arguments')

            # for check string
            column_names <- list()
            for(i in 1:length(slot_name)){
              column_names[[i]] <- colnames( hm_get(obj = obj, slot_name = slot_name[i]) )[-1]
            }
            check_string(argument = col_name_vect, target = unlist(column_names),
                         arg_name = 'col_name')



            #*interactive
            check_class(argument = interactive, target = 'logical', arg_name = 'interactive')
            check_length(argument = interactive, max_allow = 1, arg_name = 'interactive')

            #*line_type
            if( is.null(scatter) ){
              # line plots
              check_class(argument = line_type, target = 'character', arg_name = 'line_type')
              if(interactive == FALSE){
                # ggplot2
                check_string(argument = line_type, target = c('solid', 'twodash',
                                                              'longdash', 'dotted', 'dotdash', 'dashed', 'blank'),
                             arg_name = 'line_type')

              } else{
                #plotly
                check_string(argument = line_type, target = c('lines', 'lines+markers',
                                                              'markers'),
                             arg_name = 'line_type')
              } # string check
              check_cross(ref_arg = col_name_vect, eval_arg = line_type,
                          arg_names = c('col_name', 'line_type') )

            } else{
              # scatter plots
              check_class(argument = line_type, target = 'numeric', arg_name = 'line_type (point shape)')
              check_numeric(argument = line_type, target = 0:24, arg_name = 'line_type (point shape)')
              check_length(argument = line_type, max_allow = 1, arg_name = 'line_type (point shape)')

            }

            #*line_color
            check_class(argument = line_color, target = 'character', arg_name = 'line_color')
            check_string(argument = line_color, target = colors(), arg_name = 'line_color')
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_color,
                          arg_names = c('col_name', 'line_color'))
            } else{
              # scatter
              check_length(argument = line_color, max_allow = 1, arg_name = 'line_color (scatter)')
            }

            #*line_size
            check_class(argument = line_size, target = c('numeric', 'integer'),
                        arg_name = 'line_size')

            # for valid size values
            check_values <- sum( which(line_size < 0) )
            if(check_values != 0){ stop('line_size argument(s) shuld be greater than 0!',
                                        call. = FALSE) }

            # for cross validation
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_size,
                          arg_names = c('col_name', 'line_size'))
            } else{
              # scatter
              check_length(argument = line_size, max_allow = 1, arg_name = 'line_size (scatter)')
            }


            #*line_alpha
            check_class(argument = line_alpha, target = 'numeric', arg_name = 'line_alpha')

            # for valid size values
            check_values <- sum( which(line_alpha < 0 | line_alpha > 1 ) )
            if(check_values != 0){ stop('line_alpha argument(s) shuld be between 0 and 1!',
                                        call. = FALSE) }

            # for cross validation
            if( is.null(scatter) ){
              # line
              check_cross(ref_arg = col_name_vect, eval_arg = line_alpha,
                          arg_names = c('col_name', 'line_alpha'))
            } else{
              # scatter
              check_length(argument = line_alpha, max_allow = 1, arg_name = 'line_alpha (scatter)')
            }


            #* xlab
            check_class(argument = x_lab, target = 'character', arg_name = 'x_lab')
            check_length(argument = x_lab, max_allow = 1, arg_name = 'x_lab')

            #* ylab
            check_class(argument = y_lab, target = 'character', arg_name = 'y_lab')
            if( is.null(dual_yaxis) ){
              # single y axis
              check_length(argument = y_lab, max_allow = 1, arg_name = 'y_lab')

            } else{
              # dual y axis
              check_length(argument = y_lab, max_allow = 2, arg_name = 'y_lab')
            }

            #* title_lab
            check_class(argument = title_lab, target = 'character', arg_name = 'title_lab')
            check_length(argument = title_lab, max_allow = 1, arg_name = 'title_lab')


            #* legend_lab
            check_class(argument = legend_lab, target = 'character', arg_name = 'legend_lab')
            check_cross(ref_arg = col_name_vect, eval_arg = legend_lab,
                        arg_names = c('col_name', 'legend_lab'))

            #* dual_yaxis
            check_class(argument = dual_yaxis, target = 'character', arg_name = 'dual_yaxis')
            check_string(argument = dual_yaxis, target = c('left', 'right'), arg_name = 'dual_yaxis')
            check_cross(ref_arg = col_name_vect, eval_arg = dual_yaxis,
                        arg_names = c('col_name', 'dual_yaxis'))

            #* from
            check_class(argument = from, target = c('character', 'POSIXct'), arg_name = 'from')
            check_length(argument = from, max_allow = 1, arg_name = 'from')

            #* to
            check_class(argument = to, target = c('character', 'POSIXct'), arg_name = 'to')
            check_length(argument = to, max_allow = 1, arg_name = 'to')

            #* scatter
            check_class(argument = scatter, target = 'character', arg_name = 'scatter')
            check_string(argument = scatter, target = c('x', 'y'), arg_name = 'scatter')
            check_cross(ref_arg = col_name_vect, eval_arg = scatter, arg_names = 'scatter')


            #**************************
            #* set default arg values
            #**************************
            #* line_type
            if( is.null(line_type) ){

              if( is.null(scatter) ){
                # line plot

                if(interactive == FALSE){
                  # ggplot2
                  line_type <- rep('solid', length(col_name_vect))

                } else {
                  # plotly
                  line_type <- rep('lines', length(col_name_vect))

                }

              } else{
                # scatter plot
                line_type <- 16

              }


            }

            #* line_color
            if( is.null(line_color) ){

              if( is.null(scatter) ){
                # line plot
                line_color <- heat.colors(n = length(col_name_vect) )

              } else{
                # scatter plot
                line_color <- 'dodgerblue'

              }


            }

            #* line_size
            if( is.null(line_size) ){

              if( is.null(scatter) ){
                # line plot
                line_size <- rep(0.8, length(col_name_vect) )

              } else{
                # scatter plot
                line_size <- 1

              }


            }

            #* line_alpha
            if( is.null(line_alpha) ){

              if( is.null(scatter) ){
                # line plot
                line_alpha <- rep(1, length(col_name_vect) )

              } else{
                # scatter plot
                line_alpha <- 1

              }


            }

            #* y_lab
            if( !is.null(dual_yaxis) ){
              if( length(y_lab) != 2){
                y_lab <- c('y', 'y2')
              }

            }


            #**************************
            #* function
            #**************************
            #* build classic table (subsetted)
            c_table <- build_table(hm_obj = obj, slot_name = slot_name,
                                   col_name = col_name, from = from, to = to)


            #* is it interactive?
            if(interactive == FALSE){
              #* ggplot2


              #* scatter plot?
              if( is.null(scatter) == TRUE ) {
                #* line plot

                #* do we need double y axis?
                if( is.null(dual_yaxis) == TRUE ){
                  #* single y axis

                  #* transform to ggplot table
                  gg_table <- ggplot_table(df = c_table, l_color = line_color,
                                           l_type = line_type, l_size = line_size,
                                           l_legend = legend_lab)

                  #* ggplot2 object
                  gg_out <-
                    ggplot(data = gg_table,
                           aes_string(x = 'date', y = 'series', group = 'group') ) +
                    geom_line( aes_string(size = 'group', linetype = 'group', color = 'group', alpha = 'group')  ) +
                    scale_color_manual( values = line_color ) +
                    scale_linetype_manual(values = line_type ) +
                    scale_size_manual(values = line_size ) +
                    scale_alpha_manual(values = line_alpha) +
                    theme(legend.title = element_blank()) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)


                  return(gg_out)



                } else{
                  #* dual y axis


                  #* rescale the table
                  index_left  <- which(dual_yaxis == 'left')
                  index_right <- which(dual_yaxis == 'right')

                  nm_table    <- colnames(c_table)[-1] # variable names
                  left_names  <- nm_table[index_left]
                  right_names <- nm_table[index_right]

                  dual_list <- dual_y_table(df = c_table,
                                            y_left = left_names,
                                            y_right = right_names)

                  dual_table <- dual_list[['table']]

                  #* transform to ggplot table
                  gg_table <- ggplot_table(df = dual_table,
                                           l_color = line_color,
                                           l_type = line_type,
                                           l_size = line_size,
                                           l_legend = legend_lab)

                  #* get coefficients inside the environment
                  a  <- as.numeric( dual_list[['coefficients']][1] )
                  b  <- as.numeric( dual_list[['coefficients']][2] )
                  sf <- as.numeric( dual_list[['coefficients']][3] )

                  trans <- dual_list[['transformation']]


                  #* ggplot2 object
                  gg_out <-
                    ggplot(data = gg_table,
                           aes_string(x = 'date', y = 'series', group = 'group') ) +
                    geom_line( aes_string(size = 'group', linetype = 'group', color = 'group', alpha = 'group')  ) +
                    scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]) ) +
                    scale_color_manual( values = line_color ) +
                    scale_linetype_manual(values = line_type ) +
                    scale_size_manual(values = line_size ) +
                    scale_alpha_manual(values = line_alpha) +
                    theme(legend.title = element_blank()) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)


                  return(gg_out)

                }
              } else {
                #* scatter plot


                #* get x and y variable position
                index_x <- which(scatter == 'x')
                index_y <- which(scatter == 'y')

                nm_table <- colnames(c_table)[-1] # variable names
                x_name   <- nm_table[index_x]
                y_name   <- nm_table[index_y]

                #* ggplot2 object
                gg_out <-
                  ggplot(data = c_table,
                         aes(x = c_table[ , x_name], y = c_table[ , y_name]) ) +
                  geom_point(size = line_size, color = line_color, alpha = line_alpha, shape = line_type ) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)


                return(gg_out)



              }





            } else{
              #* plotly

              if( is.null(scatter) == TRUE ){
                # line plot

                # dual y axis?
                if( is.null(dual_yaxis) == TRUE ){
                  # single y axis

                  # basic plotly
                  ppout   <- plot_ly(c_table, x = ~date)
                  n_plots <- ncol(c_table) - 1

                  # build graphs without axis labels
                  for(i in 1:n_plots){
                    ppout <- ppout %>%
                      add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                line = list( width = line_size[i]), opacity = line_alpha[i] )

                  }# end for

                  # add axis labels
                  ppout <-
                    ppout %>%
                    layout(title = title_lab,
                           xaxis = list(title = x_lab),
                           yaxis = list(title = y_lab) )

                  # return
                  return(ppout)

                } else {
                  # dual y axis

                  # basic plotly
                  ppout   <- plot_ly(c_table, x = ~date)
                  n_plots <- ncol(c_table) - 1

                  # genero graficos sin etiquetas en los ejes
                  for(i in 1:n_plots){

                    if(dual_yaxis[i] == 'left'){
                      # left axis
                      ppout <- ppout %>%
                        add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                  type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                  line = list( width = line_size[i]), opacity = line_alpha[i] )

                    } else if (dual_yaxis[i] == 'right'){
                      # right axis
                      ppout <- ppout %>%
                        add_trace(y = c_table[ , (i + 1)], name = legend_lab[i],
                                  type = 'scatter', mode = line_type[i], color = I(line_color[i]),
                                  line = list( width = line_size[i]), opacity = line_alpha[i],
                                  yaxis = 'y2')

                    }


                  }# end for

                  # add labels
                  ppout <-
                    ppout %>%
                    layout(title  = title_lab,
                           xaxis  = list(title = x_lab),
                           yaxis  = list(title = y_lab[1]),
                           yaxis2 = list(title = y_lab[2],
                                         overlaying = 'y',
                                         side = 'right')  )

                  # return
                  return(ppout)

                }



              } else {
                #* scatter plot


                #* get x and y variable position
                index_x <- which(scatter == 'x')
                index_y <- which(scatter == 'y')

                nm_table <- colnames(c_table)[-1] # variable names
                x_name   <- nm_table[index_x]
                y_name   <- nm_table[index_y]

                #* ggplot2 object
                gg_out <-
                  ggplot(data = c_table,
                         aes(x = c_table[ , x_name], y = c_table[ , y_name]) ) +
                  geom_point(size = line_size, color = line_color, alpha = line_alpha, shape = line_type ) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)

                #* turn it to plotly
                pp_out <- ggplotly( gg_out )

                return(pp_out)



              }





            }


          })
