# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Create, modify and delete columns inside a slot
#'
#' @description This method allows you to modify whatever (except \code{'date'} column)
#' you want inside a slot data frame. Since this package was designed with the aim of providing
#' useful objects to store and track changes in hydro-meteorological series, is not recommend
#' to delete or change the original data, but it is upon to you.
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string with the a valid name.
#' @param FUN function name. The function output must be a data frame with
#' the first column being the \code{Date}. Note that \code{hydrotoolbox} provides common
#' used hydrological functions: see for example \link{mov_avg}. An interesting function to use is
#' \code{mutate} from \code{dplyr} package.
#' @param ... \code{FUN} arguments to pass.
#'
#' @return The same object but with the modified slot's data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # path to all example files
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # build the snih station file
#' guido <-
#' hm_create() %>%
#'   hm_build(bureau = 'snih', path = path,
#'            file_name = c('snih_hq_guido.xlsx',
#'            'snih_qd_guido.xlsx'),
#'            slot_name = c('hq', 'qd'),
#'            by = c('none', 'day') ) %>%
#'   hm_name(slot_name = 'qd',
#'           col_name = 'q(m3/s)')
#'
#'  # apply a moving average windows to streamflow records
#' hm_mutate(obj = guido, slot_name = 'qd',
#'           FUN = mov_avg, k = 10,
#'           pos = 'c', out_name = 'mov_avg') %>%
#'  hm_plot(slot_name = 'qd',
#'          col_name = list(c('q(m3/s)', 'mov_avg') ),
#'          interactive = TRUE,
#'          line_color = c('dodgerblue', 'red3'),
#'          y_lab = 'Q(m3/s)',
#'          legend_lab = c('original', 'mov_avg')  )
#'}
#'
#'
setGeneric(name = 'hm_mutate',
           def = function(obj, slot_name, FUN, ...)
           {
             standardGeneric('hm_mutate')
           })


#' @describeIn hm_mutate method for station class.
## station
setMethod(f = 'hm_mutate',
          signature = 'hydromet_station',
          definition = function(obj, slot_name, FUN, ...){

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

            check_string(argument = slot_name,
                         target = setdiff(x = slotNames("hydromet_station"),
                                          y = slotNames("hydromet")),
                         arg_name = 'slot_name')

            check_length(argument = slot_name,
                         max_allow = 1,
                         arg_name = slot_name)

            #* FUN
            check_class(argument = FUN,
                        target = 'function',
                        arg_name = 'FUN')


            #*//////////////////
            #* function
            #*//////////////////

            #* get table
            table_slot <- hm_get(obj = obj, slot_name = slot_name)

            #* apply fun
            table_mod  <- FUN(table_slot, ...)

            check_class(argument = table_mod,
                        target = c("tbl_df", "tbl", "data.frame"),
                        arg_name = 'FUN output')

            #* set new table
            out_txt <- paste0('hm_set(obj = obj,',
                              slot_name, '=', 'table_mod', ')')
            obj     <- eval( parse(text = out_txt) )

            #* return
            validObject(obj)
            return(obj)

          })



#' @describeIn hm_mutate method for compact class.
## compact
setMethod(f = 'hm_mutate',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name, FUN, ...){

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
                         target = 'compact',
                         arg_name = 'slot_name')

            check_length(argument = slot_name,
                         max_allow = 1,
                         arg_name = slot_name)

            #* FUN
            check_class(argument = FUN,
                        target = 'function',
                        arg_name = 'FUN')


            #*//////////////////
            #* function
            #*//////////////////

            #* get table
            table_slot <- hm_get(obj = obj,
                                 slot_name = slot_name)

            #* apply fun
            table_mod  <- FUN(table_slot, ...)

            check_class(argument = table_mod,
                        target = c("tbl_df", "tbl", "data.frame"),
                        arg_name = 'FUN output')

            #* set new table
            out_txt <- paste0('hm_set(obj = obj,',
                              slot_name, '=', 'table_mod', ')')
            obj     <- eval( parse(text = out_txt) )

            #* return
            validObject(obj)
            return(obj)

          })
