# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Aggregates the table inside a slot to a larger time period
#'
#' @description This method allows you to get your data temporally aggregated.
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string with the name of the slot to aggregate.
#' @param col_name string with column(s) name(s) to aggregate.
#' @param fun string with supported aggregation function name (one per \option{col_name}):
#'  \option{mean}, \option{min}, \option{max}, \option{sum}, \option{last} or \option{first}.
#' @param period string with the aggregation time-step: \option{hourly}, \option{daily},
#'  \option{monthly}, \option{annually} or \option{climatic}. \bold{NOTE 1}: the
#'  \option{climatic} option returns the all series annual statistics (\option{fun}).
#'  \bold{NOTE 2}: when using \option{annually} as \bold{period}, the method will return
#'  the starting dates in the first slot column.
#' @param out_name string with the output column(s) name(s). Default values coerce the
#' original name plus the \option{fun} argument (e.g.: \code{tair_max}).
#' @param allow_na optional. Numeric value with the maximum allowed number of \code{NA_real_}
#'  values. By default the function will not tolerate any \code{NA_real_} (and will
#'  return \code{NA_real_} instead).
#' @param start_month optional. Numeric value defining the first month of the annual
#' period (it just make sense if \option{period} is either \option{annually} or
#' \option{climatic}). Default sets to 1 (January). \bold{NOTE}: keep in mind that if
#' you choose \option{climatic} as period you have to round off a complete year (e.g.:
#' \code{..., start_month = 6, end_month = 5, ...})
#' @param end_month optional. Numeric value defining the last month of the annual period
#' (it just make sense if \option{period} is either \option{annually} or \option{climatic}).
#' Default sets to 12 (December). \bold{NOTE}: keep in mind that if you choose
#' \option{climatic} as period you have to round off a complete year (e.g.:
#' \code{..., start_month = 6, end_month = 5, ...})
#' @param relocate optional. String with the name of the slot where to allocate the
#'  aggregated table. It only make sense for \code{hydromet_station} class. When using
#'  it you must keep in mind that all aggregated series are allocated in a single slot.
#'
#' @return A data frame with the Date and the aggregated variable(s) inside the
#' specified slot.
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
#' # aggregate air temperature data to mean value
#' hm_agg(obj = hm_cuevas, slot_name = 'tair', col_name = 'tair(°C)',
#'       fun = 'mean', period = 'daily', out_name = 't_mean') %>%
#'  hm_show(slot_name = 'tair')
#'
#' # the previous command overwrites the original slot, so now we are going
#' # to relocate the agg values
#' hm_agg(obj = hm_cuevas, slot_name = 'tair',
#'       col_name = 'tair(°C)',
#'       fun = 'mean',
#'       period = 'daily',
#'       relocate = 'tmean',
#'       out_name = 'tmean(°C)',
#'       ) %>%
#'  hm_show(slot_name = 'tmean')
#'}
#'
#'
setGeneric(name = 'hm_agg',
           def = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                          allow_na = 0, start_month = 1, end_month = 12,
                          relocate = NULL)
           {
             standardGeneric('hm_agg')
           })


#' @describeIn hm_agg temporal aggregation method for station class
## station
setMethod(f = 'hm_agg',
          signature = 'hydromet_station',
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                allow_na = 0, start_month = 1, end_month = 12,
                                relocate = NULL){
            #*/////////////////
            #* conditionals
            #*/////////////////
            #* obj
            check_class(argument = obj,
                        target = 'hydromet_station',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = names( hm_show(obj = obj, slot_name = 'fill') ),
                         arg_name = 'slot_name')

            check_length(argument = slot_name,
                         max_allow = 1,
                         arg_name = 'slot_name')

            #* NOTE: remember that most of the conditionals (from here)
            #* live inside the agg_table() function

            #* col_name
            check_class(argument = col_name,
                        target = 'character',
                        arg_name = 'col_name')

            #* fun
            check_class(argument = fun,
                        target = 'character',
                        arg_name = 'fun')

            #* period
            check_class(argument = period,
                        target = 'character',
                        arg_name = 'period')

            #* out_name
            if( !is.null(out_name) ){
              check_class(argument = out_name,
                          target = 'character',
                          arg_name = 'out_name')
            }

            #* allow_na
            check_class(argument = allow_na,
                        target = 'numeric',
                        arg_name = 'allow_na')

            #* start_month
            check_class(argument = start_month,
                        target = 'numeric',
                        arg_name = 'start_month')

            #* end_month
            check_class(argument = end_month,
                        target = 'numeric',
                        arg_name = 'end_month')

            #* relocate
            if( !is.null(relocate) ){

              check_class(argument = relocate,
                          target = 'character',
                          arg_name = 'relocate')

              check_string(argument = relocate,
                           target = setdiff(x = slotNames("hydromet_station"),
                                            y = slotNames("hydromet")
                                            ),
                           arg_name = 'relocate')

              check_length(argument = relocate,
                           max_allow = 1,
                           arg_name = 'relocate')


            }

            #*///////////////
            #* function
            #*///////////////

            table_agg <-
              hm_get(obj = obj, slot_name = slot_name) %>% #* get the table
              agg_table(col_name = col_name,
                        fun = fun,
                        period = period,
                        out_name = out_name,
                        allow_na = allow_na,
                        start_month = start_month,
                        end_month = end_month) #* aggregate table

            # when period = annually agg_table returns a three column table
            if(period == "annually"){
              table_agg <- table_agg[ , -2]
            }


            #* set table in the slot
            if( is.null(relocate) ){
              # set it in the same slot
              out_txt <- paste0('hm_set(obj = obj,',
                                slot_name, '=', 'table_agg', ')')

            } else {
              # change to another slot
              out_txt <- paste0('hm_set(obj = obj,', relocate, '=', 'table_agg', ')')

            }


            out_obj <- eval( parse(text = out_txt) )

            #* return
            validObject(out_obj) # just an extra control
            return(out_obj)


          })


#' @describeIn hm_agg temporal aggregation method for compact class
## compact
setMethod(f = 'hm_agg',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                allow_na = 0, start_month = 1, end_month = 12){

            #*///////////////
            #* conditionals
            #*///////////////

            #* obj
            check_class(argument = obj,
                        target = 'hydromet_compact',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = names( hm_show(obj = obj, slot_name = 'fill') ),
                         arg_name = 'slot_name')

            check_length(argument = slot_name,
                         max_allow = 1,
                         arg_name = 'slot_name')

            #* NOTE: remember that most of the conditionals (from here)
            #* live inside the agg_table() function

            #* col_name
            check_class(argument = col_name,
                        target = 'character',
                        arg_name = 'col_name')

            #* fun
            check_class(argument = fun,
                        target = 'character',
                        arg_name = 'fun')

            #* period
            check_class(argument = period,
                        target = 'character',
                        arg_name = 'period')

            #* out_name
            if( !is.null(out_name) ){
              check_class(argument = out_name,
                          target = 'character',
                          arg_name = 'out_name')
            }

            #* allow_na
            check_class(argument = allow_na,
                        target = 'numeric',
                        arg_name = 'allow_na')

            #* start_month
            check_class(argument = start_month,
                        target = 'numeric',
                        arg_name = 'start_month')

            #* end_month
            check_class(argument = end_month,
                        target = 'numeric',
                        arg_name = 'end_month')

            #*///////////////
            #* function
            #*///////////////

            table_agg <-
              hm_get(obj = obj, slot_name = slot_name) %>% #* get the table
              agg_table(col_name = col_name,
                        fun = fun,
                        period = period,
                        out_name = out_name,
                        allow_na = allow_na,
                        start_month = start_month,
                        end_month = end_month) #* aggregate table

            # when period = annually agg_table returns a three column table
            if(period == "annually"){
              table_agg <- table_agg[ , -2]
            }

            #* set table in the slot
            out_txt <- paste0('hm_set(obj = obj,', slot_name, '=', 'table_agg', ')')
            out_obj <- eval( parse(text = out_txt) )

            #* return
            validObject(out_obj) # just an extra control
            return(out_obj)


          })
