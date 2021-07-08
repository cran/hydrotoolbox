# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Load native data files automatically
#'
#' @description The method allows you to automatically load your native data
#' inside the \code{hydromet_station} slots.
#'
#' @param obj a valid \code{hydromet_station} class object.
#' @param bureau string value containing \bold{one} of the available options:
#' 'aic', 'cr2', 'dgi', 'ianigla', 'mnemos' or 'snih'.
#' @param path string vector with the path(s) to the \code{file_name} argument.
#' If you set a single string it will be recycled for all the files.
#' @param file_name string vector with the native file(s) name(s).
#' @param slot_name string vector with the slot(s) where to set the file(s) or sheet(s).
#' @param by string vector with the time step of the series (e.g.: 'month', 'day', '6 hour', '3 hour',
#'  '1 hour', '15 min' ). If you set it as 'none', the function will ignore automatic gap filling.
#'  If you set a single string it will be recycled for all the files.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#' @param sheet optional. Sheet to read. Either a string vector (the name of a sheet), or an integer
#'  vector (the position of the sheet). If neither argument specifies the sheet, defaults to the first
#'  sheet. This argument just make sense for:
#'  \itemize{
#'  \item 'aic': you must provide a single name or integer indicating the met-station to read.
#'  \item 'dgi': just keep it as \code{NULL}.
#'  \item 'mnemos': just keep it as \code{NULL}.
#'  }
#'
#' @return A \code{hydromet_station} object with the required data loaded inside.
#'
#' @export
#'
#' @examples
#'
#' # path to all example files
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # ianigla file
#' hm_create() %>%
#'   hm_build(bureau = 'ianigla', path = path,
#'            file_name = 'ianigla_cuevas.csv',
#'            slot_name = c('tair', 'rh', 'patm',
#'                          'precip', 'wspd', 'wdir',
#'                          'kin', 'hsnow', 'tsoil'),
#'            by = 'hour',
#'            out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                         'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                         'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
#'           ) %>%
#'           hm_show()
#'
#' # cr2 file
#' hm_create() %>%
#'   hm_build(bureau = 'cr2', path = path,
#'            file_name = 'cr2_tmax_yeso_embalse.csv',
#'            slot_name = c('tmax'),
#'            by = 'day',
#'            out_name = c('tair(°C)' )
#'           )  %>%
#'           hm_show()
#'
#' # dgi file
#' hm_create() %>%
#'   hm_build(bureau = 'dgi', path = path,
#'            file_name = 'dgi_toscas.xlsx',
#'            slot_name = c('swe', 'tmax',
#'            'tmin', 'tmean', 'rh', 'patm'),
#'            by = 'day' ) %>%
#'           hm_show()
#'
#' # snih file
#' hm_create() %>%
#'   hm_build(bureau = 'snih', path = path,
#'            file_name = c('snih_hq_guido.xlsx',
#'            'snih_qd_guido.xlsx'),
#'            slot_name = c('hq', 'qd'),
#'            by = c('none', 'day') ) %>%
#'           hm_show()
#'
#' # aic    => you have to request for this files to AIC.
#'
#' # mnemos => the data are the same of snih but generated
#' #           with MNEMOSIII software.
#'
#'
setGeneric(name = 'hm_build',
           def = function(obj,
                          bureau,
                          path, file_name,
                          slot_name, by,
                          out_name = NULL,
                          sheet = NULL)
           {
             standardGeneric('hm_build')
           })

#' @describeIn hm_build build method for hydromet station object
## station
setMethod(f = 'hm_build',
          signature = 'hydromet_station',
          definition = function(obj,
                                bureau,
                                path, file_name,
                                slot_name, by,
                                out_name = NULL,
                                sheet = NULL){
            #**************************
            #* conditionals
            #**************************
            #* obj
            check_class(argument = obj, target = 'hydromet_station', arg_name = 'obj')

            #* bureau
            check_class(argument = bureau, target = 'character', arg_name = 'bureau')
            check_string(argument = bureau,
                         target = c('aic', 'cr2', 'dgi', 'ianigla', 'mnemos', 'snih'),
                         arg_name = 'bureau')
            check_length(argument = bureau, max_allow = 1, arg_name = 'bureau')

            #* file_name
            check_class(argument = file_name, target = 'character', arg_name = 'file_name')

            #* path (invert the order because length issue)
            check_class(argument = path, target = 'character', arg_name = 'path')
            check_length(argument = path,
                         max_allow = length(file_name),
                         arg_name = 'path')

            # check the file(s) existance
            check_string(argument = file_name, target = list.files(path = path),
                         arg_name = 'file_name')

            #* slot_name
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')
            check_string(argument = slot_name,
                         target = slotNames('hydromet_station')[1:23],
                         arg_name = 'slot_name')

            #* by
            check_class(argument = by, target = 'character', arg_name = 'by')

            #* out_name
            if( !is.null(out_name) ){

              check_class(argument = out_name, target = 'character', arg_name = 'out_name')

            }

            #* sheet
            if( !is.null(sheet) ){

              check_class(argument = sheet, target = c('character', 'integer'), arg_name = 'sheet')

            }

            #**************************
            #* function
            #**************************
            if( bureau == 'aic'){
              #* aic
              check_length(argument = path, max_allow = 1, arg_name = 'path (aic)') # path
              check_length(argument = file_name, max_allow = 1, arg_name = 'file_name (aic)') # file_name

              table_aic <- read_aic(path = paste0(path, '/', file_name),
                                    by = by,
                                    out_name = out_name,
                                    sheet = sheet )

              # iterate through the slot_name
              # the slot_name match column variables inside the sheet
              n_it <- length(slot_name)
              for(i in 1:n_it){

                table_sub <- table_aic[ , c(1, (i+1) )]

                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_sub', ')')
                obj     <- eval( parse(text = out_txt) )

              } # end for loop

              # return object
              validObject(object = obj)
              return(obj)

            } else if(bureau == 'cr2'){
              #* cr2
              #* file_name commands: path - slot_name - by - out_name
              n_files <- length(file_name)

              #* recycle arguments if necessary
              path <- rep(path, n_files)
              by   <- rep(by, n_files)

              #* check slot_name and out_name
              check_cross(ref_arg = file_name, eval_arg = slot_name,
                          arg_names = c('file_name', 'slot_name') )

              if( !is.null(out_name) ){

                check_cross(ref_arg = file_name, eval_arg = out_name,
                            arg_names = c('file_name', 'out_name') )

              } else {

                out_name <- rep(out_name, n_files)

              }

              for(i in 1:n_files){
                temp_path <- paste0( path[i], '/', file_name[i] )
                table_cr2 <- read_cr2(path = temp_path,
                                      by = by[i],
                                      out_name = out_name[i])

                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_cr2', ')')
                obj     <- eval( parse(text = out_txt) )

                rm(temp_path, table_cr2, out_txt)

              }# end for loop

              # return object
              validObject(object = obj)
              return(obj)


            } else if(bureau == 'dgi'){
              #* dgi
              #* check single for arguments
              check_length(argument = path, max_allow = 1, arg_name = 'path (dgi)') # path
              check_length(argument = file_name, max_allow = 1,
                           arg_name = 'file_name (dgi)') # file_name
              check_length(argument = by, max_allow = 1, arg_name = 'by (dgi)') # by

              #* get fix values
              file_path <- paste0(path, '/', file_name)

              sheet_name <- excel_sheets( path = file_path )
              n_it       <- length(sheet_name)

              #* recycle out_name if necessary
              if( !is.null(out_name) ){

                check_cross(ref_arg = sheet_name,
                            eval_arg = out_name,
                            arg_names = c('sheets', 'out_name'))

              } else{

                out_name <- rep(NULL, n_it)

              }

              #* check for slot_name
              check_cross(ref_arg = sheet_name,
                          eval_arg = slot_name,
                          arg_names = c('sheets', 'slot_name'))

              #* loop around column variables
              for(i in 1:n_it){
                #* read variable inside sheet
                table_dgi <- read_dgi(path = file_path,
                                      by = by,
                                      out_name = out_name[i],
                                      sheet = sheet_name[i] )
                #* set variable
                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_dgi', ')')
                obj     <- eval( parse(text = out_txt) )

                rm(table_dgi, out_txt)

              }# end for loop

              # return object
              validObject(object = obj)
              return(obj)

            } else if(bureau == 'ianigla'){
              #* ianigla
              check_length(argument = path, max_allow = 1, arg_name = 'path (ianigla)') # path
              check_length(argument = file_name, max_allow = 1, arg_name = 'file_name (ianigla)') # file_name

              table_ianigla <- read_ianigla(path = paste0(path, '/', file_name),
                                            by = by,
                                            out_name = out_name )


              # iterate through the slot_name
              # the slot_name match column variables inside the sheet
              n_it <- length(slot_name)
              for(i in 1:n_it){

                table_sub <- table_ianigla[ , c(1, (i+1) )]

                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_sub', ')')
                obj     <- eval( parse(text = out_txt) )

              } # end for loop

              # return object
              validObject(object = obj)
              return(obj)

            } else if(bureau == 'mnemos'){
              #* mnemos
              check_length(argument = path, max_allow = 1, arg_name = 'path (mnemos)') # path
              check_length(argument = file_name, max_allow = 1, arg_name = 'file_name (mnemos)') # file_name

              #* args fit to slot_name
              n_it <- length(slot_name)

              if( length(by) == 1 ){

                by <- rep(by, n_it)

              } else {

                check_cross(ref_arg = slot_name,
                            eval_arg = by,
                            arg_names = c('slot_name', 'by'))

              }



              if( !is.null(out_name) ){

                check_cross(ref_arg = slot_name,
                            eval_arg = out_name,
                            arg_names = c('slot_name', 'out_name'))

              } else{

                out_name <- rep(NULL, n_it )

              }

              #* get file_path
              file_path  <- paste0( path, '/', file_name )
              sheet_name <- excel_sheets(file_path) # to iterate around all sheets

              # iterate through the slot_name
              # the slot_name match column variables inside the sheet
              for(i in 1:n_it){

                table_mnemos <- read_mnemos(path = file_path,
                                            by = by[i],
                                            out_name = out_name[i],
                                            sheet = sheet_name[i]
                                            )

                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_mnemos', ')')
                obj     <- eval( parse(text = out_txt) )

                rm(table_mnemos, out_txt)

              } # end for loop

              # return object
              validObject(object = obj)
              return(obj)


            } else if(bureau == 'snih'){
              #* snih

              #* args fit to slot_name
              n_it <- length(slot_name)

              if( length(by) == 1 ){

                by <- rep(by, n_it)

              } else {

                check_cross(ref_arg = slot_name,
                            eval_arg = by,
                            arg_names = c('slot_name', 'by'))

              }

              check_cross(ref_arg = slot_name,
                          eval_arg = file_name,
                          arg_names = c('slot_name', 'file_name'))

              if( !is.null(out_name) ){

                check_cross(ref_arg = slot_name,
                            eval_arg = out_name,
                            arg_names = c('slot_name', 'out_name'))

              } else{

                out_name <- rep(NULL, n_it )

              }

              if( length(path) > 1 ){

                check_cross(ref_arg = slot_name,
                            eval_arg = path,
                            arg_names = c('slot_name', 'path_name'))

              } else{

                path <- rep(path, n_it )

              }

              # iterate through the slot_name
              # the slot_name match column variables inside the sheet
              for(i in 1:n_it){
                #* get file_path
                file_path  <- paste0( path[i], '/', file_name[i] )

                table_snih <- read_snih(path = file_path,
                                        by = by[i],
                                        out_name = out_name[i])

                out_txt <- paste0('hm_set(obj = obj,', slot_name[i], '=', 'table_snih', ')')
                obj     <- eval( parse(text = out_txt) )

                rm(table_snih, out_txt, file_path)

              } # end for loop

              # return object
              validObject(object = obj)
              return(obj)

            }



          })
