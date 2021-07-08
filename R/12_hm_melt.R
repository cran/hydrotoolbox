# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Melt many objects into an \code{hydromet_compact} class object
#'
#' @description This method allows you merge several tables (inside \code{hydromet_station} and/or
#' \code{hydromet_compact} class objects) into a single one and set them into the compact slot
#' (\code{hydromet_compact} class object).
#'
#' @param obj a valid \code{hydromet_compact} class object.
#' @param melt string vector containing the  \code{hydromet_xxx} class objects names (as you
#' have in the \bold{Global Environment}) that you want for melting.
#' @param slot_name list (one element per \code{melt} vector name) with the slot(s) to
#' extract per every \code{hydromet_xxx} class object (as string vectors).
#' @param col_name string vector with the name of the variables to keep. You must comply
#' the following name convention \code{'melt_slot_variable'} (e.g.: 'guido_qd_Q(m3/s)' - where
#' \bold{guido} is your object name, \bold{qd} is the slot with daily mean river discharge
#'  and \bold{Q(m3/s)} is the required column name inside that slot). Another option is to set
#'  this argument just with the string \code{'all'} and the method will preserve \bold{all}
#'  the slot(s) columns. Other minimal option is to choose the string \code{'last'}: in this
#'  case you will get only the \bold{last} column of each slot(s).
#' @param out_name optional. String vector with the output names of the final table. If you
#' use the default value (\code{NULL}) the method will add the object and slot name
#' (provided in \code{melt} and \code{slot_name} argument) at the beginning of every column
#' (e.g.: \code{'guido_qd_q(m3/s)'}).
#'
#' @return An \code{hydromet_compact} class object with a data frame inside the \code{compact}
#' slot with all variables that you provided in \code{col_name}.
#'
#' @note Remember that all the chosen variables should have the same temporal resolution.
#' The method itself will not warn you about bad entries.
#'
#' @export
#'
#' @examples
#'
#' # lets say that we want to put together snow water equivalent from Toscas (dgi)
#' # and daily streamflow discharge from Guido (snih)
#'
#' # path to all example files
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # on the first place we build the stations
#' # dgi file
#' toscas <-
#' hm_create() %>%
#'   hm_build(bureau = 'dgi', path = path,
#'            file_name = 'dgi_toscas.xlsx',
#'            slot_name = c('swe', 'tmax',
#'            'tmin', 'tmean', 'rh', 'patm'),
#'            by = 'day',
#'            out_name = c('swe', 'tmax',
#'            'tmin', 'tmean', 'rh', 'patm') )
#'
#' # snih file
#' guido <-
#' hm_create() %>%
#'   hm_build(bureau = 'snih', path = path,
#'            file_name = c('snih_hq_guido.xlsx',
#'            'snih_qd_guido.xlsx'),
#'            slot_name = c('hq', 'qd'),
#'            by = c('none', 'day') )
#'
#' # now we melt the requiered data
#' hm_create(class_name = 'compact') %>%
#'      hm_melt(melt = c('toscas', 'guido'),
#'              slot_name = list(toscas = 'swe', guido = 'qd'),
#'              col_name = 'all',
#'              out_name = c('swe(mm)', 'qd(m3/s)')
#'              ) %>%
#'        hm_plot(slot_name = 'compact',
#'                col_name = list( c('swe(mm)', 'qd(m3/s)') ),
#'                interactive = TRUE,
#'                line_color = c('dodgerblue', 'red'),
#'                y_lab = c('q(m3/s)', 'swe(mm)'),
#'                dual_yaxis = c('right', 'left')
#'                 )
#'
#'
#'
setGeneric(name = 'hm_melt',
           def = function(obj, melt, slot_name,
                          col_name, out_name = NULL)
           {
             standardGeneric('hm_melt')
           })


#' @describeIn hm_melt plot method for compact class
## compact
setMethod(f = 'hm_melt',
          signature = 'hydromet_compact',
          definition = function(obj, melt, slot_name,
                                col_name, out_name = NULL){

            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_compact', arg_name = 'obj')

            #* melt
            check_class(argument = melt, target = 'character', arg_name = 'melt')

            #* slot_name
            check_class(argument = slot_name, target = 'list', arg_name = 'slot_name')
            check_class(argument = unlist(slot_name),
                        target = 'character', arg_name = 'slot_name elements')
            check_string(argument = unlist(slot_name),
                         target = c('compact', slotNames('hydromet_station')[1:23] ) ,
                         arg_name = 'slot_name elements' )
            check_cross(ref_arg = melt , eval_arg = slot_name, arg_names = c('melt', 'slot_name') )

            #* col_name
            check_class(argument = col_name, target = 'character', arg_name = 'col_name')

            #* out_name
            if( !is.null(out_name) ){

              check_class(argument = out_name, target = 'character', arg_name = 'out_name')

            }


            #**************************
            #* function
            #**************************

            n_it <- length(melt)

            for(i in 1:n_it){
              #* inside melt
              hm_obj <- get(x = melt[i], pos = '.GlobalEnv')
              n_slot <- length( slot_name[[i]] )

              for(j in 1:n_slot){
                #* inside slot_name
                #* get_slot
                table_aux <- hm_get(obj = hm_obj, slot_name = slot_name[[i]][j] )


                #* set default col_names
                colnames(table_aux) <- c('date',
                                          paste0(slot_name[[i]][j], '_', colnames(table_aux)[-1] )
                                          )

                #* merge tables
                if(j == 1){

                  table_slot <- table_aux

                } else {

                  table_slot <- merge(x = table_slot, y = table_aux, all = TRUE)

                }

                rm(table_aux)


              } # end for j

              #* set default col_names
              colnames(table_slot) <- c('date',
                                       paste0(melt[i], '_', colnames(table_slot)[-1] )
              )

              #* merge them all
              if(i == 1){

                table_melt <- table_slot

              } else {

                table_melt <- merge(x = table_melt, y = table_slot, all = TRUE)

              }

              rm(table_slot, n_slot)

            } # end for i

            #* subset with col_name argument
            if( length(col_name) == 1 ){

              if( col_name == 'all'){

                table_out <- table_melt

              } else if( col_name == 'last'){

                # detect the pattern in the column names
                string_target <- colnames(table_melt)
                column_pos    <- c()
                for(i in 1:n_it){

                  # now inside the slot
                  n_j <- length( slot_name[[i]] )
                  for(j in 1:n_j){

                    string_patt   <- paste0( melt[i], '_', slot_name[[i]][j] )
                    column_pos[i] <- max(
                      grep(pattern = string_patt, x = string_target)
                    )

                    rm(string_patt)

                  } # end for j

                } # end for i

                table_out <- table_melt[ , c(1, column_pos) ]


              } else{

                table_out <- subset(x = table_melt, select = c('date', col_name) )

              }



            } else {
              # user supplied col_names
              table_out <- subset(x = table_melt, select = c('date', col_name) )



            }



            #* set out_name
            if( !is.null(out_name) ){

              # check length
              check_cross(ref_arg = colnames(table_out)[-1],
                          eval_arg = out_name,
                          arg_names = c('final table column names', 'out_name') )

              colnames(table_out) <- c('date', out_name)

            }

            #* create
            hm_out <- hm_set(obj = obj, compact = table_out)

            #* return
            validObject(hm_out)
            return(hm_out)

          })
