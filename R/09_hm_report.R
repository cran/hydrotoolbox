# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Get a summary report of your data
#'
#' @description Returns a list with two elements: the first one contains basic
#' statistics (\code{mean}, \code{sd}, \code{max} and \code{min}) values and
#' the second one is a table with summary of miss data (see also \link{report_miss}).
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string with the name of the slot to report.
#' @param col_name string vector with the column(s) name(s) to report. By default
#' the function will do it in all columns inside the slot.
#'
#' @return A list summarizing basic statistics and missing data.
#'
#' @export
#'
#' @examples
#'
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
#' # report incoming solar radiation
#' hm_report(obj = hm_cuevas, slot_name = 'kin')
#'
setGeneric(name = 'hm_report',
           def = function(obj, slot_name, col_name = 'all')
           {
             standardGeneric('hm_report')
           })

#' @describeIn hm_report report method for station class
# station
setMethod(f = 'hm_report',
          signature = 'hydromet_station',
          definition = function(obj, slot_name, col_name = 'all'){
            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_station', arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name, target = slotNames('hydromet_station')[1:23],
                         arg_name = 'slot_name')
            check_length(argument = slot_name, max_allow = 1, arg_name = 'slot_name')

            #* col_name
            check_class(argument = col_name, target = 'character', arg_name = 'col_name')
            check_string(argument = col_name, target = c('all',
                                                         colnames( hm_get(obj = obj, slot_name = slot_name ) )[-1] ),
                         arg_name = 'col_name')


            #**************************
            #* function
            #**************************
            #* get table
            table_r <- hm_get(obj = obj, slot_name = slot_name) # table to report

              #* select columns
            if(col_name == 'all'){
              col_nm <- colnames(table_r)

            } else {
              col_nm <-  c('date', col_name)

            }

            table_s  <- subset(x = table_r, select = col_nm)
            table_nr <- nrow(table_s)
            matrix_s <- as.matrix(table_s[ , -1])

              #* calculate stats
            stat_max  <- col_max(x = matrix_s, allow_na = table_nr )
            stat_min  <- col_min(x = matrix_s, allow_na = table_nr )
            stat_mean <- col_mean(x = matrix_s, allow_na = table_nr )
            stat_sd   <- col_sd(x = matrix_s, allow_na = table_nr )

            m_stat <- rbind(stat_min,
                            stat_max,
                            stat_mean,
                            stat_sd)

              #* build the data.frame
            df_stat <- data.frame(date = c( table_s[1, 1], table_s[table_nr, 1],
                                            NA_character_, NA_character_ ),
                                  m_stat)

            colnames(df_stat) <- colnames(table_s)
            rownames(df_stat) <- c('min', 'max', 'mean', 'sd')


            #* report miss
            list_miss <- report_miss(x = table_s, col_name = 'all')

            #* create list
            out_list <- list(stats = df_stat,
                             miss_data = list_miss)

            #* return
            return(out_list)

          })


#' @describeIn hm_report report method for compact class
# compact
setMethod(f = 'hm_report',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name = 'compact', col_name = 'all'){
            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_compact', arg_name = 'obj')

            #* slot_name
            slot_name <- 'compact'
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name, target = 'compact',
                         arg_name = 'slot_name')
            check_length(argument = slot_name, max_allow = 1, arg_name = 'slot_name')

            #* col_name
            check_class(argument = col_name, target = 'character', arg_name = 'col_name')
            check_string(argument = col_name, target = c('all',
                                                         colnames( hm_get(obj = obj, slot_name = slot_name ) )[-1] ),
                         arg_name = 'col_name')


            #**************************
            #* function
            #**************************
            #* get table
            table_r <- hm_get(obj = obj, slot_name = slot_name) # table to report

            #* select columns
            if(col_name == 'all'){
              col_nm <- colnames(table_r)

            } else {
              col_nm <-  c('date', col_name)

            }

            table_s  <- subset(x = table_r, select = col_nm)
            table_nr <- nrow(table_s)
            matrix_s <- as.matrix(table_s[ , -1])

            #* calculate stats
            stat_max  <- col_max(x = matrix_s, allow_na = table_nr )
            stat_min  <- col_min(x = matrix_s, allow_na = table_nr )
            stat_mean <- col_mean(x = matrix_s, allow_na = table_nr )
            stat_sd   <- col_sd(x = matrix_s, allow_na = table_nr )

            m_stat <- rbind(stat_min,
                            stat_max,
                            stat_mean,
                            stat_sd)

            #* build the data.frame
            df_stat <- data.frame(date = c( table_s[1, 1], table_s[table_nr, 1],
                                            NA_character_, NA_character_ ),
                                  m_stat)

            colnames(df_stat) <- colnames(table_s)
            rownames(df_stat) <- c('min', 'max', 'mean', 'sd')


            #* report miss
            list_miss <- report_miss(x = table_s, col_name = 'all')

            #* create list
            out_list <- list(stats = df_stat,
                             miss_data = list_miss)

            #* return
            return(out_list)

          })
