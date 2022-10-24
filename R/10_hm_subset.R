# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Subset your data by dates
#'
#' @description The method will subset the required slot.
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string vector with the name(s) of the slot(s) to subset. If you use
#' 'all' as argument the method will subset all the variables with data.
#' @param from string \code{Date} or \code{POSIX*} value with the starting date. You can
#' use \code{from} without \code{to}. In this case you will subset your data \code{from}
#' till the end.
#' @param to string \code{Date} or \code{POSIX*} value with the starting date. You can
#' use \code{to} without \code{from}. In this case you will subset your data from the
#' beginning till \code{to}.
#'
#' @return The same \code{hydromet_XXX} class object provided in \code{obj} but subsetted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # cuevas station
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
#'           )
#'
#' # subset relative humidity and plot it
#' hm_subset(obj = hm_cuevas, slot_name = 'rh',
#'           from = ISOdate(2020, 2, 1),
#'           to = ISOdate(2020, 4, 1) ) %>%
#'   hm_plot(slot_name = 'rh',
#'           col_name = list('rh(%)'),
#'           interactive = TRUE,
#'           y_lab = 'RH(%)' )
#'}
#'
setGeneric(name = 'hm_subset',
           def = function(obj, slot_name = 'all', from = NULL, to = NULL)
           {
             standardGeneric('hm_subset')
           })

#' @describeIn hm_subset subset method for station class
# station
setMethod(f = 'hm_subset',
          signature = 'hydromet_station',
          definition = function(obj, slot_name = 'all', from = NULL, to = NULL){

            #*//////////////////
            #* conditionals
            #*//////////////////
            #* obj
            check_class(argument = obj,
                        target = 'hydromet_station',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            allow_string <- setdiff(x = slotNames(x = "hydromet_station"),
                                    y = slotNames(x = "hydromet")
                                    )

            check_string(argument = slot_name,
                         target = c('all',
                                    allow_string),
                         arg_name = 'slot_name')

            # to set all fill names
            if(slot_name == 'all'){
             slot_name <- names( hm_show(obj = obj) )
            }

            #* from
            check_class(argument = from,
                        target = c('character', 'POSIXct', "POSIXlt"),
                        arg_name = 'from')

            check_length(argument = from,
                         max_allow = 1,
                         arg_name = 'from')

            #* to
            check_class(argument = to,
                        target = c('character', 'POSIXct', "POSIXlt"),
                        arg_name = 'to')

            check_length(argument = to,
                         max_allow = 1,
                         arg_name = 'to')


            #*//////////////////
            #* function
            #*//////////////////

            #* loop in the slot_name arg
            n_it <- length(slot_name)
            date <- NULL # for binding

            out_obj <- obj

            for(i in 1:n_it){
              #* get the table
              table_o <- hm_get(obj = out_obj, slot_name = slot_name[i]) # original

              #* subset
              if( !is.null(from) & !is.null(to) ){
                table_s <- subset(x = table_o, subset = date >= from & date <= to )

              } else if( !is.null(from) & is.null(to) ){
                table_s <- subset(x = table_o, subset = date >= from )

              } else if( is.null(from) & !is.null(to) ){
                table_s <- subset(x = table_o, subset = date <= to )

              }

              #* set the subsetted table
              out_txt <- paste0('hm_set(obj = out_obj,', slot_name[i], '=', 'table_s', ')')
              out_obj <- eval( parse(text = out_txt) )


              rm(table_o, table_s, out_txt)

            }


            #* return obj
            return(out_obj)

          })

#' @describeIn hm_subset subset method for compact class
# compact
setMethod(f = 'hm_subset',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name = 'all', from = NULL, to = NULL){

            #*//////////////////
            #* conditionals
            #*//////////////////
            #* obj
            check_class(argument = obj,
                        target = 'hydromet_compact',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = c('all', 'compact'),
                         arg_name = 'slot_name')

            # to set all fill names
            if(slot_name == 'all'){
              slot_name <- names( hm_show(obj = obj) )
            }

            #* from
            check_class(argument = from,
                        target = c('character', 'POSIXct', "POSIXlt"),
                        arg_name = 'from')

            check_length(argument = from,
                         max_allow = 1,
                         arg_name = 'from')

            #* to
            check_class(argument = to,
                        target = c('character', 'POSIXct', "POSIXlt"),
                        arg_name = 'to')

            check_length(argument = to,
                         max_allow = 1,
                         arg_name = 'to')


            #*//////////////////
            #* function
            #*//////////////////

            #* loop in the slot_name arg
            n_it <- length(slot_name)
            date <- NULL # for binding

            out_obj <- obj

            for(i in 1:n_it){
              #* get the table
              table_o <- hm_get(obj = out_obj, slot_name = slot_name[i]) # original

              #* subset
              if( !is.null(from) & !is.null(to) ){
                table_s <- subset(x = table_o, subset = date >= from & date <= to )

              } else if( !is.null(from) & is.null(to) ){
                table_s <- subset(x = table_o, subset = date >= from )

              } else if( is.null(from) & !is.null(to) ){
                table_s <- subset(x = table_o, subset = date <= to )

              }

              #* set the subsetted table
              out_txt <- paste0('hm_set(obj = out_obj,', slot_name[i], '=', 'table_s', ')')
              out_obj <- eval( parse(text = out_txt) )


              rm(table_o, table_s, out_txt)

            }


            #* return obj
            return(out_obj)

          })
