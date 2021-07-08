# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Set new column names
#'
#' @description Change slot's column names.
#'
#' @param obj a valid \code{hydromet_compact} class object.
#' @param slot_name string with the a valid name.
#' @param col_name string vector with new column names.
#'
#' @return The same object but with new column names.
#'
#' @export
#'
#' @examples
#'
#' # path to all example files
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # we first build the snih station file
#' guido <-
#' hm_create() %>%
#'   hm_build(bureau = 'snih', path = path,
#'            file_name = c('snih_hq_guido.xlsx',
#'            'snih_qd_guido.xlsx'),
#'            slot_name = c('hq', 'qd'),
#'            by = c('none', 'day') )
#'
#'  guido %>% hm_show(slot_name = 'qd')
#'
#' # now we can change default names
#' hm_name(obj = guido, slot_name = 'qd',
#'         col_name = 'q(m3/s)') %>%
#'         hm_show(slot_name = 'qd')
#'
setGeneric(name = 'hm_name',
           def = function(obj, slot_name, col_name)
           {
             standardGeneric('hm_name')
           })


#' @describeIn hm_name set new column name for station class
## station
setMethod(f = 'hm_name',
          signature = 'hydromet_station',
          definition = function(obj, slot_name, col_name){
            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_station', arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name,
                         target = slotNames('hydromet_station')[1:23],
                         arg_name = 'slot_name')
            check_length(argument = slot_name, max_allow = 1, arg_name = slot_name)

            #* col_name
            check_class(argument = col_name, target = 'character', arg_name = 'col_name')

            #**************************
            #* function
            #**************************
            #* get slot
            table_slot <- hm_get(obj = obj, slot_name = slot_name)
            col_nm     <- colnames(table_slot)[-1]

            check_cross(ref_arg = col_nm, eval_arg = col_name,
                        arg_names = c('slot colnames', 'col_name'))

            #* set new names
            colnames(table_slot) <- c( 'date', col_name )

            #* set slot
            out_txt <- paste0('hm_set(obj = obj,', slot_name, '=', 'table_slot', ')')
            hm_out  <- eval( parse(text = out_txt) )

            #* return
            validObject(hm_out)
            return(hm_out)

          })


#' @describeIn hm_name set new column name for compact class
## compact
setMethod(f = 'hm_name',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name, col_name){
            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_compact', arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name,
                         target = 'compact',
                         arg_name = 'slot_name')
            check_length(argument = slot_name, max_allow = 1, arg_name = slot_name)

            #* col_name
            check_class(argument = col_name, target = 'character', arg_name = 'col_name')

            #**************************
            #* function
            #**************************
            #* get slot
            table_slot <- hm_get(obj = obj, slot_name = slot_name)
            col_nm     <- colnames(table_slot)[-1]

            check_cross(ref_arg = col_nm, eval_arg = col_name,
                        arg_names = c('slot colnames', 'col_name'))

            #* set new names
            colnames(table_slot) <- c( 'date', col_name )

            #* set slot
            out_txt <- paste0('hm_set(obj = obj,', slot_name, '=', 'table_slot', ')')
            hm_out  <- eval( parse(text = out_txt) )

            #* return
            validObject(hm_out)
            return(hm_out)

          })
